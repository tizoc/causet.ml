(* Wallet send's lifecycle State and its transitions. A request ID is
   causet's [Request_id.t], made by the reducer ([Ctx.new_request]) rather
   than a counter kept in State, so it also tells two runs of the node
   apart. The State is a value: every transition returns the next one. *)

open Causet
open Domain
open Mempool

val owner_max_scan_positions : int
val owner_max_matches : int
val mempool_max_scan_positions : int
val mempool_max_matches : int

(* A snapshot- and cursor-pinned request for one bounded persisted page. *)
type collect_request = {
  request_id : Request_id.t;
  rpc_id : rpc_id;
  chunk_seq : int;
  anchor_root : root;
  owner : owner;
  next_index : int;
  max_scan_positions : int;
  max_matches : int;
  mempool_cursor : owned_outputs_cursor;
}
[@@deriving debug]

(* One persisted page, as the UTXO database answers it. *)
type owned_utxos_chunk = {
  utxos : (commitment * utxo) list;
  frontier_exclusive : int;
  next_index : int;
  complete : bool;
  scanned_positions : int;
}
[@@deriving debug]

(* A correlated request for one bounded mempool page. *)
type mempool_request = {
  request_id : Request_id.t;
  rpc_id : rpc_id;
  chunk_seq : int;
  anchor_root : root;
  cursor : owned_outputs_cursor;
  max_scan_positions : int;
  max_matches : int;
}
[@@deriving debug]

(* What proving needs, self-contained. *)
type prove_request = {
  request_id : Request_id.t;
  rpc_id : rpc_id;
  anchor_root : root;
  timestamp_ms : int;
  req : send_request;
  available_utxos : (commitment * utxo) list;
}
[@@deriving debug]

(* What follows a persisted page. *)
type persisted_transition =
  | Collect of collect_request
  | Scan_mempool of mempool_request
  | Hydrate of { anchor_root : root; selection_error : string }

(* What follows a mempool page. *)
type mempool_transition =
  | Continue
  | Prove of prove_request
  | Mempool_hydrate of { anchor_root : root; selection_error : string }

(* Deterministic correlation for the asynchronous wallet-send lifecycle. *)
type t

val empty : t
val has_work : t -> rpc_id -> bool
val owner_is_available : t -> rpc_id -> owner -> bool

(* The owner reserved for the RPC; [None] when another RPC holds it. *)
val reserve_owner : t -> rpc_id -> owner -> t option
val release_owner : t -> rpc_id -> t
val can_start : t -> rpc_id -> bool

(* The RPC no longer waits for a mempool scan. *)
val not_ready : t -> rpc_id -> t

(* A collection begun, and its first page's request. *)
val begin_collecting :
  t ->
  Request_id.t ->
  rpc_id ->
  root ->
  int ->
  send_request ->
  owned_outputs_cursor ->
  (t * collect_request, string) result

val is_collecting_persisted : t -> collect_request -> bool
val is_collecting_mempool : t -> mempool_request -> bool
val collection_matches_rpc_request : t -> rpc_id -> Request_id.t -> send_request -> bool
val collection_pins : t -> rpc_id -> Request_id.t -> (root * owned_outputs_cursor) option

(* The next owner whose mempool scan continues, round robin. [Error] when
   readiness names an RPC that is not collecting from the mempool. *)
val next_mempool_scan_request : t -> (mempool_request option, rpc_id) result

val validate_persisted_chunk :
  collect_request -> owned_utxos_chunk -> (unit, string) result

(* The State after a persisted page, and what follows; [None], the State
   unchanged, when the page is not for the current collection. *)
val apply_persisted_chunk :
  t ->
  collect_request ->
  (owned_utxos_chunk, string) result ->
  Commitment_set.t ->
  (t * persisted_transition) option

(* The State after a mempool page, and what follows; [None], the State
   unchanged, when the page is not for the current collection. *)
val apply_mempool_chunk :
  t -> mempool_request -> owned_outputs_chunk -> (t * mempool_transition) option

(* The collection ended; [None] when it is not the RPC's current one. *)
val finish_collecting : t -> rpc_id -> Request_id.t -> t option
val is_proving : t -> rpc_id -> Request_id.t -> bool
val finish_proving : t -> rpc_id -> Request_id.t -> t option

(* The State with the RPC's work gone, and whether it had any. *)
val cancel : t -> rpc_id -> t * bool

(* The RPCs with work in progress. *)
val working : t -> rpc_id list

(* The structural invariants the lifecycle relies on: the two reservation
   maps are each other's inverse, so an owner has at most one send; every
   send in progress holds its owner; and only an RPC collecting from the
   mempool is ready for a mempool scan. *)
val invariants : t -> (unit, string) result
