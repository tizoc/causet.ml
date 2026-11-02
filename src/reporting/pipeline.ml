(* The pipeline that turns reports into records: dedupe, rate limits,
   sampling and metric aggregation. Which reports it keeps, and which
   samples a reservoir holds, are decided by BLAKE3 hashes of the reports,
   as in the Rust port, so both keep the same ones. *)

open Report

(* An [f32], as the Rust port holds sampling rates. *)
let f32 x = Int32.float_of_bits (Int32.bits_of_float x)
let clamp01 x = if Float.is_nan x then x else Float.min 1. (Float.max 0. x)

(* [floor x] as a [u64], in an [Int64], for [0 <= x < 2^64]. *)
let u64_of_float x =
  if Float.is_nan x then 0L
  else if x >= 0x1p63 then Int64.add (Int64.of_float (x -. 0x1p63)) Int64.min_int
  else Int64.of_float x

let saturating_sub a b = if a > b then a - b else 0

(* ------------------------------------------------------------------------ *)
(* Hashing: the bytes the Rust port feeds its hasher, tagged and
   length-prefixed, so that no two structures hash alike. They go straight
   into the pipeline's hasher, piece by piece, as they do in the Rust
   port. *)

open Payload
module H = Blake3

(* A hash as dedupe keys it: the 32-byte digest, then the report's kind. *)
let key_len = 33

let kind_code = function
  | Error -> '\000'
  | Metric -> '\001'
  | Event -> '\002'
  | Healthcheck -> '\003'

let digest_key h kind =
  let key = Bytes.create key_len in
  H.digest_into h key 0;
  Bytes.unsafe_set key 32 (kind_code kind);
  Bytes.unsafe_to_string key

let add_tagged h tag s =
  H.add_string h tag;
  H.add_int_le h (String.length s);
  H.add_string h s

let hash_value h = function
  | Null -> H.add_string h "|n|"
  | Bool b ->
      H.add_string h "|b|";
      H.add_char h (if b then '\001' else '\000')
  | Uint n ->
      H.add_string h "|u|";
      H.add_int_le h n
  | Int n ->
      H.add_string h "|i|";
      H.add_int_le h n
  | Float x ->
      H.add_string h "|f|";
      H.add_float_bits_le h x
  | String s -> add_tagged h "|s|" s
  | Raw_json s -> add_tagged h "|j|" s

let rec hash_object h t =
  H.add_string h "|object|";
  H.add_int_le h (List.length t.fields);
  H.add_int_le h (List.length t.sections);
  H.add_int_le h (List.length t.arrays);
  hash_fields h t.fields;
  hash_sections h t.sections;
  hash_arrays h t.arrays

and hash_fields h = function
  | [] -> ()
  | (k, v) :: rest ->
      add_tagged h "|k|" k;
      hash_value h v;
      hash_fields h rest

and hash_sections h = function
  | [] -> ()
  | (k, s) :: rest ->
      add_tagged h "|section|" k;
      hash_object h s;
      hash_sections h rest

and hash_arrays h = function
  | [] -> ()
  | (k, a) :: rest ->
      add_tagged h "|array|" k;
      hash_items h a;
      hash_arrays h rest

and hash_items h items =
  H.add_string h "|items|";
  H.add_int_le h (List.length items);
  hash_item_list h items

and hash_item_list h = function
  | [] -> ()
  | item :: rest ->
      (match item with
      | Object o ->
          H.add_string h "|obj|";
          hash_object h o
      | Array a ->
          H.add_string h "|arr|";
          hash_items h a
      | Value v ->
          H.add_string h "|val|";
          hash_value h v);
      hash_item_list h rest

(* The hash sampling and reservoirs read: the report's kind, source, dedupe
   key and payload. *)
let report_hash h (report : Report.t) =
  H.add_string h (kind_name report.kind);
  add_tagged h "|source|" report.source;
  (match report.dedupe_key with Some key -> add_tagged h "|dedupe|" key | None -> ());
  hash_object h report.payload;
  digest_key h report.kind

(* With a dedupe key, dedupe compares it, the kind, source and event. *)
let dedupe_key_hash h (report : Report.t) key =
  H.add_string h (kind_name report.kind);
  add_tagged h "|source|" report.source;
  add_tagged h "|event|" (Option.value (Payload.event_str report.payload) ~default:"");
  add_tagged h "|key|" key;
  digest_key h report.kind

(* A report's two hashes, each computed the first time a step reads it:
   dedupe (the dedupe key's hash, or the payload's), sampling at a rate
   strictly between 0 and 1, and a full reservoir (the payload's). [""]
   is one not computed yet. *)
type hashes = { mutable payload_hash : string; mutable dedupe_hash : string }

let no_hashes () = { payload_hash = ""; dedupe_hash = "" }

let payload_hash h hashes report =
  if String.length hashes.payload_hash = 0 then
    hashes.payload_hash <- report_hash h report;
  hashes.payload_hash

let dedupe_hash h hashes (report : Report.t) =
  if String.length hashes.dedupe_hash = 0 then
    hashes.dedupe_hash <-
      (match report.dedupe_key with
      | None -> payload_hash h hashes report
      | Some key -> dedupe_key_hash h report key);
  hashes.dedupe_hash

(* ------------------------------------------------------------------------ *)
(* Queues: rings of ints and strings, which grow and are never shrunk, so a
   push allocates nothing once one has its size. *)

module Ring = struct
  type 'a t = {
    mutable data : 'a array;
    mutable head : int;
    mutable len : int;
    dummy : 'a;
  }

  let create dummy = { data = [||]; head = 0; len = 0; dummy }
  let length r = r.len
  let is_empty r = r.len = 0

  let grow r =
    let cap = Array.length r.data in
    let data = Array.make (Int.max 16 (2 * cap)) r.dummy in
    for i = 0 to r.len - 1 do
      Array.unsafe_set data i (Array.unsafe_get r.data ((r.head + i) land (cap - 1)))
    done;
    r.data <- data;
    r.head <- 0

  let push r x =
    if r.len = Array.length r.data then grow r;
    Array.unsafe_set r.data ((r.head + r.len) land (Array.length r.data - 1)) x;
    r.len <- r.len + 1

  (* The oldest; the ring must not be empty. *)
  let front r = Array.unsafe_get r.data r.head

  (* The slot keeps its value until a push overwrites it: at most a ring's
     worth of values outlive their entries. *)
  let drop_front r =
    r.head <- (r.head + 1) land (Array.length r.data - 1);
    r.len <- r.len - 1
end

(* ------------------------------------------------------------------------ *)
(* Metric aggregation *)

(* One aggregated field over a window; kept, emptied, from one window to the
   next. *)
type field_aggregate = {
  name : string;
  mutable count : int;
  mutable sum : float;
  mutable min : float;
  mutable max : float;
  mutable samples : Float.Array.t;
  mutable n_samples : int;
}

(* One [group_by] field's counts over a window: present once a report has
   the field, even if its value is not counted. *)
type group = {
  group : string;
  mutable present : bool;
  values : (string, int ref) Hashtbl.t;
}

(* Which reservoir slot the [count]th value replaces, if any: decided by the
   report's hash and [count]. *)
let reservoir_index h hash count cap =
  if count = 0 || cap = 0 then -1
  else (
    H.add_substring h hash 0 32;
    H.add_int_le h count;
    let value = H.digest_u64_prefix h in
    let idx =
      if count <= 1 lsl 30 then
        (* [value mod count], unsigned, from its two 32-bit halves. *)
        let hi = Int64.to_int (Int64.shift_right_logical value 32)
        and lo = Int64.to_int value land 0xffff_ffff in
        (((hi mod count) lsl 32) + lo) mod count
      else Int64.to_int (Int64.unsigned_rem value (Int64.of_int count))
    in
    if idx < cap then idx else -1)

let add_value h a value hash cap =
  a.count <- a.count + 1;
  a.sum <- a.sum +. value;
  if a.count = 1 then (
    a.min <- value;
    a.max <- value)
  else (
    if value < a.min then a.min <- value;
    if value > a.max then a.max <- value);
  if cap > 0 then
    if a.n_samples < cap then (
      if a.n_samples = Float.Array.length a.samples then (
        let samples = Float.Array.make (Int.min cap (Int.max 16 (2 * a.n_samples))) 0. in
        Float.Array.blit a.samples 0 samples 0 a.n_samples;
        a.samples <- samples);
      Float.Array.unsafe_set a.samples a.n_samples value;
      a.n_samples <- a.n_samples + 1)
    else
      let i = reservoir_index h (hash ()) a.count cap in
      if i >= 0 && i < a.n_samples then Float.Array.unsafe_set a.samples i value

(* The order samples are sorted in, the same in every port: by value, a NaN
   after every number and NaNs tied, -0. and 0. tied. [Float.compare] puts a
   NaN first. *)
let compare_samples a b =
  match (Float.is_nan a, Float.is_nan b) with
  | false, false -> if a < b then -1 else if a > b then 1 else 0
  | true, true -> 0
  | true, false -> 1
  | false, true -> -1

(* The nearest-rank [percent]th percentile of the sorted samples: the
   ceil(percent/100 * n)-th smallest, counting from 1, clamped to [1, n]. *)
let percentile sorted percent =
  match Float.Array.length sorted with
  | 0 -> None
  | n ->
      let rank = Int.max 1 (Int.min n (((percent * n) + 99) / 100)) in
      Some (Float.Array.get sorted (rank - 1))

let field_object a o =
  let open Payload in
  let opt = function Some x -> Float x | None -> Null in
  let sorted = Float.Array.sub a.samples 0 a.n_samples in
  Float.Array.stable_sort compare_samples sorted;
  o |> field "count" (Uint a.count) |> field "min" (Float a.min)
  |> field "max" (Float a.max)
  |> field "avg" (if a.count = 0 then Null else Float (a.sum /. float_of_int a.count))
  |> field "p50" (opt (percentile sorted 50))
  |> field "p95" (opt (percentile sorted 95))
  |> array_values "samples"
       (List.init a.n_samples (fun i -> Float (Float.Array.get a.samples i)))

(* The window of one policy's source and event. Its fields and groups are
   made with it, one per distinct name, in name order; [field_slots] and
   [group_slots] follow the configuration's lists, a repeated name sharing
   its aggregate. *)
type window = {
  source : string;
  event : string;
  agg : Config.Aggregate.t;
  mutable start_ms : int option;
  fields : field_aggregate array;
  field_slots : field_aggregate array;
  groups : group array;
  group_slots : group array;
}

let make_window source event (agg : Config.Aggregate.t) =
  let fields =
    List.sort_uniq String.compare agg.fields
    |> List.map (fun name ->
           {
             name;
             count = 0;
             sum = 0.;
             min = 0.;
             max = 0.;
             samples = Float.Array.create 0;
             n_samples = 0;
           })
    |> Array.of_list
  in
  let groups =
    List.sort_uniq String.compare agg.group_by
    |> List.map (fun group -> { group; present = false; values = Hashtbl.create 8 })
    |> Array.of_list
  in
  let find_field name = Array.find_opt (fun a -> a.name = name) fields |> Option.get in
  let find_group name = Array.find_opt (fun g -> g.group = name) groups |> Option.get in
  {
    source;
    event;
    agg;
    start_ms = None;
    fields;
    field_slots = Array.of_list (List.map find_field agg.fields);
    groups;
    group_slots = Array.of_list (List.map find_group agg.group_by);
  }

let has_data (w : window) =
  Array.exists (fun a -> a.count > 0) w.fields
  || Array.exists (fun g -> g.present) w.groups

let reset (w : window) now_ms =
  w.start_ms <- Some now_ms;
  Array.iter
    (fun a ->
      a.count <- 0;
      a.sum <- 0.;
      a.min <- 0.;
      a.max <- 0.;
      a.n_samples <- 0)
    w.fields;
  Array.iter
    (fun g ->
      g.present <- false;
      Hashtbl.clear g.values)
    w.groups

let record_group g value max_groups =
  g.present <- true;
  match Hashtbl.find g.values value with
  | n -> incr n
  | exception Not_found ->
      if Hashtbl.length g.values < max_groups then Hashtbl.add g.values value (ref 1)

let aggregate_report (w : window) ~start_ms ~window_ms =
  let open Payload in
  let fields =
    Array.to_list w.fields
    |> List.filter (fun a -> a.count > 0)
    |> List.map (fun a -> (a.name, a))
  in
  let groups =
    Array.to_list w.groups
    |> List.filter (fun g -> g.present)
    |> List.map (fun g ->
           ( g.group,
             Hashtbl.fold (fun value n acc -> (value, !n) :: acc) g.values []
             |> List.sort (fun (a, _) (b, _) -> String.compare a b) ))
  in
  let payload =
    create "metric_aggregate"
    |> field "window_ms" (Uint window_ms)
    |> field "window_start_ms" (Uint start_ms)
    |> section "metric" (fun m ->
           m |> field "source" (String w.source) |> field "event" (String w.event))
    |> section_objects "fields" fields (fun o a -> field_object a o)
    |> section_objects "groups" groups (fun g values ->
           List.fold_left (fun g (value, count) -> field value (Uint count) g) g values)
    |> finish
  in
  Report.make Metric ~source:Report.pipeline payload

(* A value as text, as aggregation groups count it: [unknown] without one. *)
let group_text = function
  | Null | Raw_json _ -> "unknown"
  | Bool b -> if b then "true" else "false"
  | Uint n | Int n -> string_of_int n
  | Float x -> Payload.float_to_string x
  | String s -> s

(* ------------------------------------------------------------------------ *)
(* The pipeline *)

(* Sources and events: short names, hashed here rather than by a C call. *)
module Str_tbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal

  (* FNV-1a. *)
  let hash s =
    let h = ref 0x4bf29ce484222325 in
    for i = 0 to String.length s - 1 do
      h := !h lxor Char.code (String.unsafe_get s i) * 0x100000001b3
    done;
    !h land max_int
end)

(* Dedupe's keys are BLAKE3 digests: their first eight bytes already tell
   them apart. *)
module Seen = Hashtbl.Make (struct
  type t = string

  external get64u : string -> int -> int64 = "%caml_string_get64u"

  let equal = String.equal
  let hash key = Int64.to_int (get64u key 0) land max_int
end)

type resolved = { sample_rate : float option; window : window option }

(* A key's last sighting and the generation of its newest entry in
   [dedupe_order], so that a queue entry evicts its key only while it is
   that newest one. *)
type sighting = { mutable seen_ms : int; mutable newest : int }

type t = {
  mutable config : Config.t;
      (** Read on every report, except its metric policies, indexed by [create]. *)
  stats : Stats.t;
  rate_limits : int Ring.t Str_tbl.t;
  dedupe : sighting Seen.t;
  order_ms : int Ring.t;
  order_generation : int Ring.t;
  order_key : string Ring.t;
  mutable dedupe_generation : int;
  windows : window array;  (** One per aggregating policy, in source and event order. *)
  policies : resolved Str_tbl.t Str_tbl.t;  (** By source, then event. *)
  hasher : H.t;
  mutable next_id : int;
  run_id : string;
}

let base62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

(* The time in nanoseconds since the Unix epoch, in base62. *)
let generate_run_id () =
  let rec digits n acc =
    if n = 0 then acc else digits (n / 62) (base62.[n mod 62] :: acc)
  in
  let nanos = Int64.to_int (Int64.of_float (Unix.gettimeofday () *. 1e9)) in
  match digits nanos [] with [] -> "0" | ds -> String.of_seq (List.to_seq ds)

(* A pipeline with nothing seen yet. A later policy for the same source and
   event replaces an earlier one. *)
let create ?(config = Config.default) () =
  let last = Hashtbl.create 8 in
  List.iter
    (fun (p : Config.Policy.t) -> Hashtbl.replace last (p.source, p.event) p)
    config.metrics;
  let policies = Str_tbl.create 8 in
  let windows =
    Hashtbl.fold (fun key p acc -> (key, p) :: acc) last []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.filter_map (fun ((source, event), (p : Config.Policy.t)) ->
           let window = Option.map (make_window source event) p.aggregate in
           let by_event =
             match Str_tbl.find_opt policies source with
             | Some t -> t
             | None ->
                 let t = Str_tbl.create 4 in
                 Str_tbl.replace policies source t;
                 t
           in
           Str_tbl.replace by_event event
             { sample_rate = Option.map clamp01 p.sample_rate; window };
           window)
    |> Array.of_list
  in
  {
    config;
    stats = Stats.create ();
    rate_limits = Str_tbl.create 8;
    dedupe = Seen.create 64;
    order_ms = Ring.create 0;
    order_generation = Ring.create 0;
    order_key = Ring.create "";
    dedupe_generation = 0;
    windows;
    policies;
    hasher = H.create ();
    next_id = 1;
    run_id = generate_run_id ();
  }

let config t = t.config
let set_config t config = t.config <- config
let stats t = t.stats
let is_enabled t = t.config.enabled

(* Tells this pipeline's records from another run's; only the first record
   carries it. *)
let run_id t = t.run_id

(* The metric policy of a metric or event report, looked up once per
   report: sampling and aggregation both read it. *)
let policy t (report : Report.t) =
  match report.kind with
  | Error | Healthcheck -> None
  | Metric | Event -> (
      match Payload.event_str report.payload with
      | None -> None
      | Some event -> (
          match Str_tbl.find (Str_tbl.find t.policies report.source) event with
          | p -> Some p
          | exception Not_found -> None))

let evict_front t =
  if not (Ring.is_empty t.order_key) then (
    let generation = Ring.front t.order_generation and key = Ring.front t.order_key in
    Ring.drop_front t.order_ms;
    Ring.drop_front t.order_generation;
    Ring.drop_front t.order_key;
    match Seen.find t.dedupe key with
    | s when s.newest = generation -> Seen.remove t.dedupe key
    | _ -> ()
    | exception Not_found -> ())

(* Records the sighting; true unless it repeats one within the window. *)
let check_dedupe t now_ms key =
  let window_ms = t.config.dedupe.window_ms in
  while
    (not (Ring.is_empty t.order_ms))
    && saturating_sub now_ms (Ring.front t.order_ms) > window_ms
  do
    evict_front t
  done;
  let generation = t.dedupe_generation in
  t.dedupe_generation <- generation + 1;
  let duplicate =
    match Seen.find t.dedupe key with
    | s ->
        let duplicate = saturating_sub now_ms s.seen_ms <= window_ms in
        s.seen_ms <- now_ms;
        s.newest <- generation;
        duplicate
    | exception Not_found ->
        Seen.add t.dedupe key { seen_ms = now_ms; newest = generation };
        false
  in
  Ring.push t.order_ms now_ms;
  Ring.push t.order_generation generation;
  Ring.push t.order_key key;
  while Ring.length t.order_key > t.config.dedupe.max_entries do
    evict_front t
  done;
  not duplicate

let check_rate_limit t now_ms (report : Report.t) =
  let { Config.Rate_limit.window_ms; max_per_window } = t.config.rate_limit in
  let seen =
    match Str_tbl.find t.rate_limits report.source with
    | q -> q
    | exception Not_found ->
        let q = Ring.create 0 in
        Str_tbl.replace t.rate_limits report.source q;
        q
  in
  while
    (not (Ring.is_empty seen)) && saturating_sub now_ms (Ring.front seen) > window_ms
  do
    Ring.drop_front seen
  done;
  if Ring.length seen >= max_per_window then false
  else (
    Ring.push seen now_ms;
    true)

(* The bound, an unsigned 64-bit integer, that a report's hash must not
   pass to be kept at [rate], strictly between 0 and 1: computed in [f32],
   as the Rust port does. *)
let threshold rate = u64_of_float (f32 (f32 rate *. f32 0x1p64))

let sampling_rate t (report : Report.t) policy =
  f32
    (match policy with
    | Some { sample_rate = Some r; _ } -> r
    | _ -> clamp01 (Config.Sampling.rate_for t.config.sampling report.kind))

(* Whether sampling keeps the report: the payload hash is read only at a
   rate strictly between 0 and 1. *)
let check_sampling t report policy hashes =
  let rate = sampling_rate t report policy in
  if rate >= 1. then true
  else if rate <= 0. then false
  else
    let hash = payload_hash t.hasher hashes report in
    Int64.unsigned_compare (Blake3.u64_prefix hash) (threshold rate) <= 0

let accept_hashed t now_ms (report : Report.t) ~policy ~hashes ~skip_sampling =
  let s = t.stats in
  if
    Config.Dedupe.applies t.config.dedupe report.kind
    && not (check_dedupe t now_ms (dedupe_hash t.hasher hashes report))
  then (
    s.dropped_deduped <- s.dropped_deduped + 1;
    None)
  else if not (check_rate_limit t now_ms report) then (
    s.dropped_rate_limited <- s.dropped_rate_limited + 1;
    None)
  else if (not skip_sampling) && not (check_sampling t report policy hashes) then (
    s.dropped_sampled <- s.dropped_sampled + 1;
    None)
  else (
    s.accepted <- s.accepted + 1;
    let id = t.next_id in
    t.next_id <- id + 1;
    Some
      {
        Record.id;
        timestamp_ms = now_ms;
        report;
        run_id = (if id = 1 then Some t.run_id else None);
      })

let accept_internal t now_ms report =
  accept_hashed t now_ms report ~policy:None ~hashes:(no_hashes ()) ~skip_sampling:true

(* Accepts a report the pipeline made itself, such as a flushed aggregate,
   without sampling or aggregating it. *)
let accept_aggregate_report t now_ms report = accept_internal t now_ms report

let rec has_number payload = function
  | [] -> false
  | f :: rest -> (
      match Payload.find_field payload f with
      | Some (Bool _ | Uint _ | Int _ | Float _) -> true
      | _ -> has_number payload rest)

let rec has_any payload = function
  | [] -> false
  | f :: rest -> Option.is_some (Payload.find_field payload f) || has_any payload rest

(* Feeds the report to its policy's window, and returns the window's
   aggregate if the report closed it. *)
let aggregate_metric t now_ms (report : Report.t) policy hashes =
  match policy with
  | Some { window = Some w; _ } ->
      let agg = w.agg and payload = report.payload in
      if not (has_number payload agg.fields || has_any payload agg.group_by) then None
      else
        let flush =
          match w.start_ms with
          | Some start -> saturating_sub now_ms start >= agg.window_ms
          | None -> false
        in
        let out =
          if flush then (
            let out =
              if has_data w then
                let start_ms = Option.value w.start_ms ~default:now_ms in
                Some (aggregate_report w ~start_ms ~window_ms:agg.window_ms)
              else None
            in
            reset w now_ms;
            out)
          else (
            if Option.is_none w.start_ms then w.start_ms <- Some now_ms;
            None)
        in
        let hash () = payload_hash t.hasher hashes report in
        List.iteri
          (fun i f ->
            let a = w.field_slots.(i) in
            match Payload.find_field payload f with
            | Some (Bool b) ->
                add_value t.hasher a (if b then 1. else 0.) hash agg.samples
            | Some (Uint n | Int n) ->
                add_value t.hasher a (float_of_int n) hash agg.samples
            | Some (Float x) -> add_value t.hasher a x hash agg.samples
            | _ -> ())
          agg.fields;
        List.iteri
          (fun i f ->
            match Payload.find_field payload f with
            | Some v -> record_group w.group_slots.(i) (group_text v) agg.max_groups
            | None -> ())
          agg.group_by;
        out
  | _ -> None

(* Feeds the report to its metric policy's aggregation, then runs it
   through dedupe, the rate limit and sampling at [now_ms], counting the
   outcome. The aggregate of a window the report closed skips sampling. *)
let accept_report t now_ms report =
  if not t.config.enabled then (
    t.stats.dropped_disabled <- t.stats.dropped_disabled + 1;
    { record = None; aggregate = None })
  else
    let policy = policy t report in
    let hashes = no_hashes () in
    let aggregate =
      Option.bind (aggregate_metric t now_ms report policy hashes) (fun a ->
          accept_internal t now_ms a)
    in
    let record = accept_hashed t now_ms report ~policy ~hashes ~skip_sampling:false in
    { record; aggregate }

(* A [metric_aggregate] report for each window that has lasted its
   [window_ms] by [now_ms] and holds data, in source and event order; each
   such window starts again at [now_ms]. None while disabled. The reports
   are not records yet: pass them to [accept_aggregate_report]. *)
let flush_aggregates t now_ms =
  if not t.config.enabled then []
  else
    Array.fold_left
      (fun out (w : window) ->
        match w.start_ms with
        | Some start when saturating_sub now_ms start >= w.agg.window_ms ->
            let out =
              if has_data w then
                aggregate_report w ~start_ms:start ~window_ms:w.agg.window_ms :: out
              else out
            in
            reset w now_ms;
            out
        | _ -> out)
      [] t.windows
    |> List.rev
