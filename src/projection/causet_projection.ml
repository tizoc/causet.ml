(* Projections of an ordered log of records into derived facts: the
   receiving side of observability, where stored reports are turned into the
   facts an application queries or alerts on.

   - A projection folds records, in log order, into outputs, each with a
     key, keeping recent context bounded by a retention window of event
     time. It follows the determinism guide: ordered collections, no I/O, no
     wall clock; time is the records' own.
   - A [Projector] feeds each record to its projections, and after a restart
     warms their context up from the stored window, dropping what that would
     output.
   - [check_restarts] checks the property restarts rely on: after a restart,
     a projection outputs exactly what an uninterrupted pass did.
   - [check_incremental] checks the property incremental projections rely
     on: what a projection stores for a key, having recomputed only what
     each record changed, is what recomputing it from every record gives.

   Storage, the cursor, checkpoints and delivery are the application's, and
   so are the contracts that keep them sound. A record's outputs are stored
   before, or atomically with, the cursor's advance past it, and processing
   resumes strictly after the cursor. A receiver that stores outputs before
   advancing the cursor writes some again after a crash, which is safe only
   if its writes are duplicate-safe and restart equivalence holds. A restart
   rebuilds context, never history, so what a warm-up outputs is dropped. A
   rebuild recomputes outputs only where their records are still stored:
   from the log's beginning, or from a checkpoint. *)

(* A projection: its name, how far back in event time its context reaches
   ([None]: it keeps none, and is not warmed up), and what it derives from
   one record, given in order. Its State lives in the closure. *)
type ('r, 'o) projection = {
  name : string;
  retention_ms : int option;
  process : 'r -> 'o list;
}

let projection ?retention_ms ~name process = { name; retention_ms; process }

(* Feeds records to projections, in the order they were added, and warms
   them up after a restart. [time] is a record's event time, in
   milliseconds: when it happened, not when it arrived. *)
module Projector = struct
  type ('r, 'o) slot = { p : ('r, 'o) projection; mutable warm_from : int option }
  type ('r, 'o) t = { time : 'r -> int; mutable slots : ('r, 'o) slot list }

  let create ~time = { time; slots = [] }

  (* Adds a projection after those already added. *)
  let add t p = t.slots <- t.slots @ [ { p; warm_from = None } ]
  let names t = List.map (fun s -> s.p.name) t.slots

  (* What every projection derives from a record, in projection order. *)
  let process t record = List.concat_map (fun s -> s.p.process record) t.slots

  (* The longest retention of any projection. *)
  let max_retention_ms t =
    List.fold_left
      (fun acc s ->
        match (acc, s.p.retention_ms) with
        | None, r -> r
        | a, None -> a
        | Some a, Some b -> Some (max a b))
      None t.slots

  (* Starts a warm-up after a restart: [last_time_ms] is the latest event
     time among the records up to the cursor. Each projection's window
     starts at [last_time_ms] minus its retention. Returns the earliest of
     them, from which the application reads its stored log to feed [warm];
     [None] when no projection keeps context. *)
  let warm_up_from t last_time_ms =
    List.iter
      (fun s ->
        s.warm_from <- Option.map (fun r -> max 0 (last_time_ms - r)) s.p.retention_ms)
      t.slots;
    List.fold_left
      (fun acc s ->
        match (acc, s.warm_from) with
        | None, w -> w
        | a, None -> a
        | Some a, Some b -> Some (min a b))
      None t.slots

  (* Feeds a stored record during a warm-up, in log order, to the
     projections whose window it falls in, and drops what they derive: a
     replayed record may lack context older than the window, so its outputs
     can differ from those stored, and must not overwrite them. *)
  let warm t record =
    let time = t.time record in
    List.iter
      (fun s ->
        match s.warm_from with
        | Some from when time >= from -> ignore (s.p.process record)
        | _ -> ())
      t.slots

  (* The whole warm-up over the records up to the cursor, for a log held in
     memory. *)
  let warm_up t log =
    match List.map t.time log with
    | [] -> ()
    | times ->
        if warm_up_from t (List.fold_left max min_int times) <> None then
          List.iter (warm t) log
end

(* Where a restarted pass first differs from the uninterrupted one. *)
type 'o difference = {
  restart : int option;
      (** How many records the latest restart before it had processed; [None] when the two
          passes differ without any restart: the projections are not deterministic. *)
  record : int;  (** The index of the record whose outputs differ. *)
  expected : 'o list;
  actual : 'o list;
}

let pp_difference pp f d =
  (match d.restart with
  | Some at -> Format.fprintf f "after a restart at %d, " at
  | None -> Format.fprintf f "without a restart, ");
  let list =
    Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp
  in
  Format.fprintf f "record %d derived [%a] instead of [%a]" d.record list d.actual list
    d.expected

(* Restart equivalence, with the application's own restart: [restarted] is
   given the records up to the cursor and returns the projector ready to go
   on. Runs [records] once without a restart and once restarting before each
   index in [restarts], and returns the first record after a restart whose
   outputs differ. *)
let check_restarts_with ?(equal = ( = )) fresh restarted records restarts =
  let uninterrupted = fresh () in
  let expected = List.map (Projector.process uninterrupted) records in
  let rec go projector restart index records expected =
    match (records, expected) with
    | record :: records, expected_here :: expected ->
        let projector, restart =
          if List.mem index restarts then
            (restarted (List.filteri (fun i _ -> i < index) all_records), Some index)
          else (projector, restart)
        in
        let actual = Projector.process projector record in
        if
          List.length actual = List.length expected_here
          && List.for_all2 equal actual expected_here
        then go projector restart (index + 1) records expected
        else Error { restart; record = index; expected = expected_here; actual }
    | _ -> Ok ()
  and all_records = records in
  go (fresh ()) None 0 records expected

(* Restart equivalence, with a restart set up the default way: a fresh
   projector warmed up from the records before it. *)
let check_restarts ?equal fresh records restarts =
  check_restarts_with ?equal fresh
    (fun prefix ->
      let projector = fresh () in
      Projector.warm_up projector prefix;
      projector)
    records restarts

(* Where what a projection stores for a key differs from what recomputing it
   from every record gives. *)
type 'o mismatch = {
  m_record : int;  (** The index of the record after which they differ. *)
  key : string;
  stored : 'o;  (** The key's last output: what the application stores. *)
  recomputed : 'o;  (** The key's output recomputed from every record so far. *)
}

let pp_mismatch pp f m =
  Format.fprintf f "after record %d, %s is stored as %a, but every record so far gives %a"
    m.m_record m.key pp m.stored pp m.recomputed

(* Incremental equals from scratch: a projection that recomputes, after each
   record, only the keys it judges the record to change must store for every
   key what recomputing it from every record would give.

   Feeds each record to [projection] and to [reference] (usually the same
   projection keeping everything it sees), keeps each key's last output as
   the application stores it, and after each record asks [recompute] for
   each stored key's output now; [recompute] closes over both, to say
   whether the projection still maintains the key and to recompute it from
   the reference. [None] skips the key. Returns the first stored output that
   differs. Only keys the projection has output are compared: one that
   outputs nothing passes, so a test checks that the keys expected are
   output separately. *)
let check_incremental ?(equal = ( = )) ~key ~projection ~reference ~recompute records =
  let module M = Map.Make (String) in
  let rec go stored index = function
    | [] -> Ok ()
    | record :: rest -> (
        let out = projection record in
        reference record;
        let stored = List.fold_left (fun m o -> M.add (key o) o m) stored out in
        let mismatch =
          M.fold
            (fun k last acc ->
              match acc with
              | Some _ -> acc
              | None -> (
                  match recompute k with
                  | Some now when not (equal now last) ->
                      Some { m_record = index; key = k; stored = last; recomputed = now }
                  | _ -> None))
            stored None
        in
        match mismatch with Some m -> Error m | None -> go stored (index + 1) rest)
  in
  go M.empty 0 records
