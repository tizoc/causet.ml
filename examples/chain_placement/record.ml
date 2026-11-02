(* The log's records: what nodes report, typed. A record has its sender, the
   sender's run, an event ID unique within the run, and its event time. *)

(* Who reported a record: a node, in one run of it. Compared field by field,
   in this order. *)
type event_identity = { sender : string; run_id : string; event_id : int }
[@@deriving debug]

(* A produced block, as its producer reports it. *)
type produced_block_observation = {
  produced_at_ms : int;
  block_hash : string;
  parent_hash : string option;
  block_height : int;
  global_slot : int;
}
[@@deriving debug]

(* A block a node adopted as its best tip. *)
type chain_block_context_observation = {
  block_hash : string;
  parent_hash : string option;
  height : int;
  global_slot : int;
}

type event =
  | Produced of produced_block_observation
      (** A node produced a block and injected it. *)
  | Tip_adopted of chain_block_context_observation
      (** A node adopted a block as its best tip. *)
  | Other  (** Anything else: chain placement ignores it. *)

type t = {
  sender : string;
  run_id : string;
  event_id : int;
  timestamp_ms : int;  (** When it happened, by the sender's clock. *)
  event : event;
}

let identity (r : t) = { sender = r.sender; run_id = r.run_id; event_id = r.event_id }

(* Its event time, for the projector: a time before 1970 reads as 0. *)
let time_ms (r : t) = max 0 r.timestamp_ms
