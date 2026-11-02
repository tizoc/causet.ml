(* Logging and execution statistics as observers.

   - [Logger]: one log message per execution on the [causet.exec] source, at
     the level the message chooses, with the fields it selects, captured at
     start; refused Actions and inline calls too.
   - [Stats]: per-type, per-kind counts and durations on one combined
     execution timeline.
   - [log_at]: application logs stamped with the scheduler time, for
     reducers and handlers, which never read system time. *)

open Causet

(* The source every execution is logged on. *)
let src = Logs.Src.create "causet.exec" ~doc:"causet executions"

module Log = (val Logs.src_log src : Logs.LOG)

let level_of = function
  | Message.Trace | Message.Debug -> Logs.Debug
  | Message.Info -> Logs.Info
  | Message.Warn -> Logs.Warning
  | Message.Error -> Logs.Error

let fields payload =
  match Message.log_fields payload with
  | [] -> ""
  | fs -> " fields=" ^ String.concat " " (List.map (fun (n, v) -> n ^ "=" ^ v) fs)

let label_part = function None -> "" | Some l -> " label=" ^ l

let refused_name = function
  | Observe.Root_guard -> "root-guard"
  | Observe.Guard -> "guard"
  | Observe.Component -> "component"
  | Observe.Absent -> "absent"

let sent_name = function
  | Observe.Root -> "root"
  | Observe.From_effect _ -> "effect"
  | Observe.Queued _ -> "queued"
  | Observe.From_inline _ -> "inline"

(* Logs every execution as it starts, with the scheduler time, the optional
   label (a node's name, say), the category, type, kind, destination path,
   metadata and the message's selected fields. A refused Action is logged
   with what refused it and where it came from; a child's Action reduced
   inline with [within], its parent's execution, at that execution's time,
   and [root_guard_skipped] when its mount has a root guard the call did not run. *)
let logger ?label () =
  let log payload fmt = Log.msg (level_of (Message.log_level payload)) fmt in
  let execution (e : Observe.Execution.t) payload =
    log payload (fun m ->
        m "time=%d%s category=%s type=%s kind=%s path=%s id=%d causal=%d depth=%d%s%s"
          e.meta.time (label_part label)
          (Meta.category_name e.category)
          e.message e.kind e.path e.meta.id
          (Meta.causal_execution_id e.meta)
          e.meta.depth
          (match e.answers with None -> "" | Some a -> Printf.sprintf " answers=%d" a)
          (fields payload))
  in
  {
    Observe.none with
    action_start = execution;
    effect_start = execution;
    event_start = execution;
    action_refused =
      (fun (r : Observe.Refusal.t) payload ->
        log payload (fun m ->
            m "time=%d%s category=action type=%s kind=%s path=%s refused=%s sent=%s%s%s"
              r.time (label_part label) r.message r.kind r.path (refused_name r.by)
              (sent_name r.sent)
              (match Observe.sent_execution r.sent with
              | None -> ""
              | Some c -> Printf.sprintf " causal=%d" c)
              (fields payload)));
    inline_action =
      (fun (i : Observe.Inline_call.t) payload ->
        log payload (fun m ->
            m "time=%d%s category=action type=%s kind=%s path=%s within=%d%s%s" i.time
              (label_part label) i.message i.kind i.path i.within
              (if i.root_guard_skipped then " root_guard_skipped=true" else "")
              (fields payload)));
    bug =
      (fun (r : Observe.Bug_report.t) ->
        Log.err (fun m ->
            m "bug condition: %s%s at %s:%d%s" r.message (label_part label) r.file r.line
              (match r.fields with
              | [] -> ""
              | fs -> " " ^ String.concat " " (List.map (fun (n, v) -> n ^ "=" ^ v) fs))));
  }

(* Logs at [level] stamped with a scheduler time, never the system clock:
   [log_at Logs.Info ~time:meta.time "stale job %d" id]. *)
let log_at level ~time fmt =
  Format.kasprintf (fun s -> Log.msg level (fun m -> m "time=%d %s" time s)) fmt

(* Calls and durations in one duration range, in nanoseconds. *)
type range_stats = { total_calls : int; total_duration : int; max_duration : int }

let empty_range = { total_calls = 0; total_duration = 0; max_duration = 0 }

(* Charged durations in buckets from up to 1 µs to over 50 ms. *)
module Duration_ranges = struct
  type t = range_stats array (* nine ranges *)

  let bounds =
    [| 1_000; 10_000; 50_000; 100_000; 500_000; 1_000_000; 5_000_000; 50_000_000 |]

  let names =
    [|
      "under_1_us";
      "under_10_us";
      "under_50_us";
      "under_100_us";
      "under_500_us";
      "under_1_ms";
      "under_5_ms";
      "under_50_ms";
      "above_50_ms";
    |]

  let create () = Array.make 9 empty_range

  let index duration =
    let rec go i =
      if i = Array.length bounds then i
      else if duration <= bounds.(i) then i
      else go (i + 1)
    in
    go 0

  (* Charges one execution of [duration] nanoseconds to its range. *)
  let add t duration =
    let i = index duration in
    let r = t.(i) in
    t.(i) <-
      {
        total_calls = r.total_calls + 1;
        total_duration = r.total_duration + duration;
        max_duration = max r.max_duration duration;
      }

  let merge t other =
    Array.iteri
      (fun i o ->
        let r = t.(i) in
        t.(i) <-
          {
            total_calls = r.total_calls + o.total_calls;
            total_duration = r.total_duration + o.total_duration;
            max_duration = max r.max_duration o.max_duration;
          })
      other

  let calls t = Array.fold_left (fun a r -> a + r.total_calls) 0 t
  let total_duration t = Array.fold_left (fun a r -> a + r.total_duration) 0 t
  let max_duration t = Array.fold_left (fun a r -> max a r.max_duration) 0 t
end

(* Execution counts and durations by category, type and kind. Durations use
   one combined timeline: each execution, whatever its category, is charged
   from its start until the next execution starts. An execution is counted
   when it is charged, so the latest one stays out until another starts. *)
module Stats = struct
  type key = Meta.category * string * string

  type t = {
    mutable last : (key * int) option;
    by_kind : (key, Duration_ranges.t) Hashtbl.t;
  }

  let create () = { last = None; by_kind = Hashtbl.create 64 }

  let start t (e : Observe.Execution.t) =
    let key = (e.category, e.message, e.kind) in
    let now = e.meta.time in
    (match t.last with
    | Some (previous, started) ->
        let ranges =
          match Hashtbl.find_opt t.by_kind previous with
          | Some r -> r
          | None ->
              let r = Duration_ranges.create () in
              Hashtbl.replace t.by_kind previous r;
              r
        in
        Duration_ranges.add ranges (max 0 (now - started))
    | None -> ());
    t.last <- Some (key, now)

  let observer t =
    {
      Observe.none with
      action_start = (fun e _ -> start t e);
      effect_start = (fun e _ -> start t e);
      event_start = (fun e _ -> start t e);
    }

  (* The statistics so far: for each category, by type then kind, sorted. *)
  type snapshot = {
    actions : (string * (string * Duration_ranges.t) list) list;
    effects : (string * (string * Duration_ranges.t) list) list;
    events : (string * (string * Duration_ranges.t) list) list;
  }

  let snapshot t =
    let of_category c =
      Hashtbl.fold
        (fun (cat, ty, kind) r acc -> if cat = c then ((ty, kind), r) :: acc else acc)
        t.by_kind []
      |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.fold_left
           (fun acc ((ty, kind), r) ->
             let kinds = Option.value (List.assoc_opt ty acc) ~default:[] in
             (ty, kinds @ [ (kind, Array.copy r) ]) :: List.remove_assoc ty acc)
           []
      |> List.sort (fun (a, _) (b, _) -> compare a b)
    in
    {
      actions = of_category Meta.Action;
      effects = of_category Meta.Effect;
      events = of_category Meta.Event;
    }
end
