(** What the application hands the pipeline, and the records it makes. *)

(** What a report is; sampling rates and dedupe are set per kind. *)
type kind = Error | Metric | Event | Healthcheck

val kind_name : kind -> string
(** As JSON writes it: [error], [metric], [event] or [healthcheck]. *)

val pipeline : string
(** The pipeline's own source, [pipeline]: flushed metric aggregates. *)

type t = {
  kind : kind;
  source : string;  (** Where it comes from: rate limits and metric policies key on it. *)
  payload : Payload.t;  (** Its [event] field selects a metric policy. *)
  dedupe_key : string option;
      (** When set, dedupe compares it, with the kind, source and event, instead of the
          whole payload. *)
}

val make : ?dedupe_key:string -> kind -> source:string -> Payload.t -> t

val to_json : t -> string
(** One JSON object of [kind], [source] and [payload]. *)

(** A report the pipeline accepted, numbered and timestamped. *)
module Record : sig
  type report = t

  type t = {
    id : int;  (** Position in the order the pipeline accepted reports, from 1. *)
    timestamp_ms : int;  (** The time it was accepted at. *)
    report : report;
    run_id : string option;  (** The pipeline's run ID, on its first record only. *)
  }

  val write_json : Buffer.t -> t -> unit
  (** The dump's line: one JSON object of [id], [timestamp_ms], [kind], [source] and
      [payload], with [run_id] first in the payload when set. *)

  val to_json : t -> string
end

(** The pipeline's counts, one per outcome. *)
module Stats : sig
  type t = {
    mutable accepted : int;  (** Reports made into records, aggregates included. *)
    mutable dropped_disabled : int;  (** While the pipeline was disabled. *)
    mutable dropped_rate_limited : int;  (** Over their source's rate limit. *)
    mutable dropped_deduped : int;  (** Seen again within the dedupe window. *)
    mutable dropped_sampled : int;  (** Left out by sampling. *)
    mutable dropped_invalid : int;
        (** Kept for the Rust port's shape: it counts payloads it could not encode, which
            cannot happen here. *)
  }

  val create : unit -> t
end

type outcome = { record : Record.t option; aggregate : Record.t option }
(** What accepting one report makes: the record, if it was kept, and the aggregate of a
    window it closed. *)
