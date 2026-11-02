(* What the pipeline and the file dump read. Every section has defaults, so
   an empty JSON object is a valid configuration. Delivery and the
   application's own checks are the application's. *)

(* The version of the envelope an application puts delivered records in.
   Nothing here reads it. *)
let envelope_version = 1

(* JSON lines written by [Filesystem], rotated by size. Nothing is written
   while [dir] is empty. *)
module Dump = struct
  type t = {
    enabled : bool;  (** Default true. *)
    dir : string;
        (** Created on the first write. Empty, the default, turns the dump off. *)
    filename : string;
        (** Appended to; a rotated file gets [-<timestamp_ms>] before its extension.
            Default [reports.jsonl]. *)
    max_file_size_bytes : int;
        (** Rotates before the next write past it; 0 never. Default 10 MiB. *)
  }

  let default =
    {
      enabled = true;
      dir = "";
      filename = "reports.jsonl";
      max_file_size_bytes = 10 * 1024 * 1024;
    }
end

(* At most [max_per_window] reports per source in any [window_ms]. *)
module Rate_limit = struct
  type t = { window_ms : int; max_per_window : int }

  let default = { window_ms = 60_000; max_per_window = 300 }
end

(* The share of reports kept, per kind, decided by the report's hash so the
   same report is always kept or always dropped. Rates are clamped to
   [0..1] and, as in the Rust port, used at [f32] precision. *)
module Sampling = struct
  type t = { error : float; metric : float; event : float; healthcheck : float }

  let default = { error = 1.0; metric = 0.1; event = 1.0; healthcheck = 1.0 }

  let rate_for t (kind : Report.kind) =
    match kind with
    | Error -> t.error
    | Metric -> t.metric
    | Event -> t.event
    | Healthcheck -> t.healthcheck
end

(* Drops a report seen within [window_ms], for the kinds switched on. *)
module Dedupe = struct
  type t = {
    enabled : bool;
        (** [default] turns it on; a JSON [dedupe] section without it leaves it off, as in
            the Rust port. *)
    window_ms : int;
        (** How long after its last sighting a report counts as a repeat; every sighting
            starts it again. Default 5 seconds. *)
    max_entries : int;
        (** Most sightings remembered, the oldest forgotten first. Default 1024. *)
    error : bool;  (** Default true. *)
    metric : bool;  (** Default false. *)
    event : bool;  (** Default false. *)
    healthcheck : bool;  (** Default false. *)
  }

  let default =
    {
      enabled = true;
      window_ms = 5_000;
      max_entries = 1_024;
      error = true;
      metric = false;
      event = false;
      healthcheck = false;
    }

  let applies t (kind : Report.kind) =
    t.enabled
    &&
    match kind with
    | Error -> t.error
    | Metric -> t.metric
    | Event -> t.event
    | Healthcheck -> t.healthcheck
end

(* Aggregates the numeric [fields] (count, min, max, average, p50, p95 and
   a reservoir of samples) and counts the values of the [group_by] fields
   over [window_ms], reported as one [metric_aggregate] report from
   [Report.pipeline]. *)
module Aggregate = struct
  type t = {
    window_ms : int;  (** From the first report a window holds. Default 30 seconds. *)
    fields : string list;
    group_by : string list;
    max_groups : int;  (** Distinct values counted per field in a window. Default 64. *)
    samples : int;
        (** Each field's reservoir, which p50 and p95 are read from by nearest rank (the
            ceil(p * n)-th smallest of the n samples); 0 keeps none. Default 128. *)
  }

  let default =
    { window_ms = 30_000; fields = []; group_by = []; max_groups = 64; samples = 128 }
end

(* Sampling and aggregation for the metric and event reports of one source
   whose payload's [event] is [event]. A later policy for the same pair
   replaces an earlier one. *)
module Policy = struct
  type t = {
    source : string;
    event : string;
    sample_rate : float option;  (** Replaces the kind's rate. *)
    aggregate : Aggregate.t option;
        (** Sees each report before dedupe, rate limits and sampling. *)
  }
end

type t = {
  enabled : bool;
      (** When false, the pipeline drops every report and flushes no aggregates. Default
          true. *)
  dump : Dump.t;
  rate_limit : Rate_limit.t;
  sampling : Sampling.t;
  metrics : Policy.t list;  (** In JSON, [metrics.policies]. *)
  dedupe : Dedupe.t;
}

let default =
  {
    enabled = true;
    dump = Dump.default;
    rate_limit = Rate_limit.default;
    sampling = Sampling.default;
    metrics = [];
    dedupe = Dedupe.default;
  }

(* ------------------------------------------------------------------------ *)
(* Reading JSON, as serde reads the Rust port's: absent fields take their
   defaults, unknown ones are ignored. *)

exception Invalid of string

let invalid fmt = Format.kasprintf (fun s -> raise (Invalid s)) fmt

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `Null -> None
  | _ -> invalid "expected an object around %s" key

let get key default read json =
  match member key json with None | Some `Null -> default | Some v -> read key v

let bool key = function `Bool b -> b | _ -> invalid "%s: expected a boolean" key
let string key = function `String s -> s | _ -> invalid "%s: expected a string" key

let uint key = function
  | `Int n when n >= 0 -> n
  | _ -> invalid "%s: expected a non-negative integer" key

let number key = function
  | `Int n -> float_of_int n
  | `Float x -> x
  | _ -> invalid "%s: expected a number" key

let strings key = function
  | `List l -> List.map (string key) l
  | _ -> invalid "%s: expected a list of strings" key

let dump _ j =
  let d = Dump.default in
  {
    Dump.enabled = get "enabled" d.enabled bool j;
    dir = get "dir" d.dir string j;
    filename = get "filename" d.filename string j;
    max_file_size_bytes = get "max_file_size_bytes" d.max_file_size_bytes uint j;
  }

let rate_limit _ j =
  let d = Rate_limit.default in
  {
    Rate_limit.window_ms = get "window_ms" d.window_ms uint j;
    max_per_window = get "max_per_window" d.max_per_window uint j;
  }

let sampling _ j =
  let d = Sampling.default in
  {
    Sampling.error = get "error" d.error number j;
    metric = get "metric" d.metric number j;
    event = get "event" d.event number j;
    healthcheck = get "healthcheck" d.healthcheck number j;
  }

let dedupe _ j =
  let d = Dedupe.default in
  {
    Dedupe.enabled = get "enabled" false bool j;
    window_ms = get "window_ms" d.window_ms uint j;
    max_entries = get "max_entries" d.max_entries uint j;
    error = get "error" d.error bool j;
    metric = get "metric" d.metric bool j;
    event = get "event" d.event bool j;
    healthcheck = get "healthcheck" d.healthcheck bool j;
  }

let aggregate _ j =
  let d = Aggregate.default in
  {
    Aggregate.window_ms = get "window_ms" d.window_ms uint j;
    fields = get "fields" [] strings j;
    group_by = get "group_by" [] strings j;
    max_groups = get "max_groups" d.max_groups uint j;
    samples = get "samples" d.samples uint j;
  }

let required key read j =
  match member key j with Some v -> read key v | None -> invalid "missing field %s" key

let policy j =
  {
    Policy.source = required "source" string j;
    event = required "event" string j;
    sample_rate = get "sample_rate" None (fun k v -> Some (number k v)) j;
    aggregate = get "aggregate" None (fun k v -> Some (aggregate k v)) j;
  }

let policies _ j =
  get "policies" []
    (fun key -> function
      | `List l -> List.map policy l | _ -> invalid "%s: expected a list" key)
    j

(* A configuration from JSON. *)
let of_json (j : Yojson.Safe.t) =
  match
    {
      enabled = get "enabled" true bool j;
      dump = get "dump" Dump.default dump j;
      rate_limit = get "rate_limit" Rate_limit.default rate_limit j;
      sampling = get "sampling" Sampling.default sampling j;
      metrics = get "metrics" [] policies j;
      dedupe = get "dedupe" Dedupe.default dedupe j;
    }
  with
  | config -> Ok config
  | exception Invalid message -> Error message

let of_string s =
  match Yojson.Safe.from_string s with
  | j -> of_json j
  | exception Yojson.Json_error message -> Error message
