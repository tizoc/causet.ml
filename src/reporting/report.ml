(* What the application hands the pipeline, and the records it makes. *)

(* What a report is; sampling rates and dedupe are set per kind. *)
type kind = Error | Metric | Event | Healthcheck

let kind_name = function
  | Error -> "error"
  | Metric -> "metric"
  | Event -> "event"
  | Healthcheck -> "healthcheck"

(* The pipeline's own source: flushed metric aggregates. *)
let pipeline = "pipeline"

type t = {
  kind : kind;
  source : string;  (** Where it comes from: rate limits and metric policies key on it. *)
  payload : Payload.t;  (** Its [event] field selects a metric policy. *)
  dedupe_key : string option;
      (** When set, dedupe compares it, with the kind, source and event, instead of the
          whole payload. *)
}

let make ?dedupe_key kind ~source payload = { kind; source; payload; dedupe_key }

let write_kind_source buf t =
  Buffer.add_string buf "\"kind\":";
  Payload.write_string buf (kind_name t.kind);
  Buffer.add_string buf ",\"source\":";
  Payload.write_string buf t.source;
  Buffer.add_string buf ",\"payload\":"

(* One JSON object of [kind], [source] and [payload]. *)
let to_json t =
  let buf = Buffer.create 128 in
  Buffer.add_char buf '{';
  write_kind_source buf t;
  Payload.write_json buf t.payload;
  Buffer.add_char buf '}';
  Buffer.contents buf

(* A report the pipeline accepted, numbered and timestamped. *)
module Record = struct
  type report = t

  type t = {
    id : int;  (** Position in the order the pipeline accepted reports, from 1. *)
    timestamp_ms : int;  (** The time it was accepted at. *)
    report : report;
    run_id : string option;  (** The pipeline's run ID, on its first record only. *)
  }

  (* The dump's line: one JSON object of [id], [timestamp_ms], [kind],
     [source] and [payload], with [run_id] first in the payload when set. *)
  let write_json buf t =
    Buffer.add_string buf
      (Printf.sprintf "{\"id\":%d,\"timestamp_ms\":%d," t.id t.timestamp_ms);
    write_kind_source buf t.report;
    Payload.write_json ?run_id:t.run_id buf t.report.payload;
    Buffer.add_char buf '}'

  let to_json t =
    let buf = Buffer.create 256 in
    write_json buf t;
    Buffer.contents buf
end

(* The pipeline's counts, one per outcome. *)
module Stats = struct
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

  let create () =
    {
      accepted = 0;
      dropped_disabled = 0;
      dropped_rate_limited = 0;
      dropped_deduped = 0;
      dropped_sampled = 0;
      dropped_invalid = 0;
    }
end

(* What accepting one report makes: the record, if it was kept, and the
   aggregate of a window it closed. *)
type outcome = { record : Record.t option; aggregate : Record.t option }
