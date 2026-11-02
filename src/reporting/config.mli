(** What the pipeline and the file dump read. Every section has defaults, so an empty JSON
    object is a valid configuration. Delivery and the application's own checks are the
    application's. *)

val envelope_version : int
(** The version of the envelope an application puts delivered records in. Nothing here
    reads it. *)

(** JSON lines written by [Filesystem], rotated by size. Nothing is written while [dir] is
    empty. *)
module Dump : sig
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

  val default : t
end

(** At most [max_per_window] reports per source in any [window_ms]. *)
module Rate_limit : sig
  type t = { window_ms : int; max_per_window : int }

  val default : t
end

(** The share of reports kept, per kind, decided by the report's hash so the same report
    is always kept or always dropped. Rates are clamped to [0..1] and, as in the Rust
    port, used at [f32] precision. *)
module Sampling : sig
  type t = { error : float; metric : float; event : float; healthcheck : float }

  val default : t

  val rate_for : t -> Report.kind -> float
  (** The rate for [kind], as configured. *)
end

(** Drops a report seen within [window_ms], for the kinds switched on. *)
module Dedupe : sig
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

  val default : t

  val applies : t -> Report.kind -> bool
  (** Whether dedupe applies to [kind]. *)
end

(** Aggregates the numeric [fields] (count, min, max, average, p50, p95 and a reservoir of
    samples) and counts the values of the [group_by] fields over [window_ms], reported as
    one [metric_aggregate] report from [Report.pipeline]. *)
module Aggregate : sig
  type t = {
    window_ms : int;  (** From the first report a window holds. Default 30 seconds. *)
    fields : string list;
    group_by : string list;
    max_groups : int;  (** Distinct values counted per field in a window. Default 64. *)
    samples : int;
        (** Each field's reservoir, which p50 and p95 are read from by nearest rank (the
            ceil(p * n)-th smallest of the n samples); 0 keeps none. Default 128. *)
  }

  val default : t
end

(** Sampling and aggregation for the metric and event reports of one source whose
    payload's [event] is [event]. A later policy for the same pair replaces an earlier
    one. *)
module Policy : sig
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

val default : t

val of_json : Yojson.Safe.t -> (t, string) result
(** A configuration from JSON, as serde reads the Rust port's: absent fields take their
    defaults, unknown ones are ignored. *)

val of_string : string -> (t, string) result
