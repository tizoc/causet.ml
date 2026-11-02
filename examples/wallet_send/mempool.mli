(* The part of the mempool wallet send reads: which commitments its
   transactions spend, and a bounded, cursor-pinned scan of one owner's
   unspent outputs. *)

open Domain

(* A stable cursor for one owner's scan, valid for one mempool generation at
   one chain tip. *)
type owned_outputs_cursor

val pp_owned_outputs_cursor : Format.formatter -> owned_outputs_cursor -> unit
val cursor_owner : owned_outputs_cursor -> owner

type owned_outputs_chunk = {
  items : (commitment * utxo) list;
  next : owned_outputs_cursor option;
  scanned_positions : int;
}
[@@deriving debug]

type owned_outputs_read_error = Invalid_limits | Stale [@@deriving debug]
type t

val create : block_hash -> root -> t

(* The UTXO root the mempool is built on. *)
val tip_root : t -> root
val with_tip_root : t -> root -> t

(* Accepts a transaction: its inputs are spent, its outputs indexed. *)
val accept : t -> commitment list -> utxo list -> t

(* A submitted transaction enters the mempool. *)
val submit : t -> submitted_tx -> t

(* A new chain tip. *)
val set_tip : t -> block_hash -> t
val has_spend : t -> commitment -> bool
val begin_owned_scan : t -> owner -> owned_outputs_cursor
val validate_cursor : t -> owned_outputs_cursor -> (unit, owned_outputs_read_error) result

(* One bounded page of the owner's unspent outputs, at most
   [max_scan_positions] inspected and [max_matches] returned. The cursor
   advances over every inspected position, spent ones included. *)
val owned_outputs_chunk :
  t ->
  owned_outputs_cursor ->
  int ->
  int ->
  (owned_outputs_chunk, owned_outputs_read_error) result
