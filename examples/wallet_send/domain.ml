(* The domain values wallet send works with, without cryptography. *)

open Causet

(* An [Ok] or an [Error], as Rust prints a [Result]. *)
let pp_result pp_ok pp_error f = function
  | Ok v -> Format.fprintf f "Ok(%a)" pp_ok v
  | Error e -> Format.fprintf f "Err(%a)" pp_error e

(* A set as Rust prints a [BTreeSet]: [{a, b}]. *)
let pp_set pp f items =
  Format.fprintf f "{%a}"
    (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp)
    items

(* A map as Rust prints a [BTreeMap]: [{k: v, ...}]. *)
let pp_map pp_key pp_value f bindings =
  pp_set (fun f (k, v) -> Format.fprintf f "%a: %a" pp_key k pp_value v) f bindings

(* A wallet owner (a public key's hash). *)
type owner = Owner of int [@@unboxed] [@@deriving debug]

(* A UTXO's commitment: a hash of the UTXO, a 64-bit word ordered unsigned,
   as the Rust port's [u64]. *)
type commitment = Commitment of int64 [@@unboxed]

let pp_commitment f (Commitment c) = Format.fprintf f "Commitment(%Lu)" c

(* The UTXO tree's root. *)
type root = Root of int64 [@@unboxed]

let pp_root f (Root r) = Format.fprintf f "Root(%Lu)" r

(* A block's hash. *)
type block_hash = Block_hash of int64 [@@unboxed]

let pp_block_hash f (Block_hash h) = Format.fprintf f "BlockHash(%Lu)" h

(* An RPC's identity. *)
type rpc_id = Rpc_id of int [@@unboxed] [@@deriving debug]

let unsigned (a : int64) b = Int64.unsigned_compare a b

module Owner_ord = struct
  type t = owner

  let compare (Owner a) (Owner b) = Int.compare a b
end

module Commitment_ord = struct
  type t = commitment

  let compare (Commitment a) (Commitment b) = unsigned a b
end

module Root_ord = struct
  type t = root

  let compare (Root a) (Root b) = unsigned a b
end

module Rpc_ord = struct
  type t = rpc_id

  let compare (Rpc_id a) (Rpc_id b) = Int.compare a b
end

module Owner_map = Map.Make (Owner_ord)
module Commitment_map = Map.Make (Commitment_ord)
module Root_map = Map.Make (Root_ord)
module Rpc_map = Map.Make (Rpc_ord)
module Int_map = Map.Make (Int)
module Request_map = Map.Make (Request_id)

module Commitment_set = struct
  include Set.Make (Commitment_ord)

  let pp f s = pp_set pp_commitment f (elements s)
end

module Rpc_set = Set.Make (Rpc_ord)
module Int_set = Set.Make (Int)

(* The base currency's token. *)
let base_token = 0

(* A transaction takes at most this many inputs. *)
let max_inputs = 124

(* Who may spend a UTXO. *)
type recipient = Key of owner | Script [@@deriving debug]

type utxo = {
  recipient : recipient;
  token : int;
  amount : int;
  data : int;  (** Attached data; spendable base-currency UTXOs carry none. *)
  nonce : int;
}
[@@deriving debug]

(* A plain base-currency UTXO of [owner]. *)
let base owner amount nonce =
  { recipient = Key owner; token = base_token; amount; data = 0; nonce }

(* Its commitment: FNV-1a over its fields' little-endian bytes, as the Rust
   port computes it. *)
let commitment utxo =
  let kind, owner =
    match utxo.recipient with Key (Owner o) -> (1L, Int64.of_int o) | Script -> (2L, 0L)
  in
  let words =
    [
      kind;
      owner;
      Int64.of_int utxo.token;
      Int64.of_int utxo.amount;
      Int64.of_int utxo.data;
      Int64.of_int utxo.nonce;
    ]
  in
  let hash =
    List.fold_left
      (fun hash word ->
        let rec bytes hash i =
          if i = 8 then hash
          else
            let byte = Int64.logand (Int64.shift_right_logical word (8 * i)) 0xffL in
            bytes (Int64.mul (Int64.logxor hash byte) 0x0100_0000_01b3L) (i + 1)
        in
        bytes hash 0)
      0xcbf2_9ce4_8422_2325L words
  in
  Commitment hash

(* A key UTXO must carry an amount. *)
let ensure_key_utxo_nonzero_amount utxo =
  match utxo.recipient with
  | Key _ when utxo.amount = 0 -> Error "key utxo with zero amount"
  | _ -> Ok ()

(* [a + b], or [None] past the largest amount. Amounts are native ints, 63
   bits, where the Rust port's are [u64]. *)
let checked_add a b = if a > max_int - b then None else Some (a + b)
let saturating_add a b = Option.value (checked_add a b) ~default:max_int

(* A wallet-send RPC request. *)
type send_request = {
  from : owner;
  to_ : owner;
  amount : int;
  fee : int option;
  memo : string;
}
[@@deriving debug]

(* A wallet-send RPC's answer. *)
type send_response = { queued : bool; error : string option; tx_id : int option }
[@@deriving debug]

let rejected error = { queued = false; error = Some error; tx_id = None }

(* A proved transaction, ready for the mempool. *)
type submitted_tx = { hash : int; inputs : commitment list; outputs : utxo list }
[@@deriving debug]

(* A bug report's field, as the Rust port's [bug_condition!] gives it. *)
let rpc_field rpc_id = [ ("rpc_id", Debug.to_string pp_rpc_id rpc_id) ]
