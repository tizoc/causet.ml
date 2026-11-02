(* The UTXO ledger as a fixed-depth Merkle tree, and what a light node asks
   a full peer for: some of its UTXOs by leaf index, each with its proof.

   The hash is small and not cryptographic, but the tree is real: a peer
   that alters an item, or answers for another index, gives a proof that
   does not lead to the root. The hashes are the Rust port's, bit for bit. *)

open Domain

(* The tree's depth: [leaves] leaves. *)
let depth = 8
let leaves = 1 lsl depth

(* The ledger: a UTXO at some of its leaves. *)
type t = { leaves : utxo Int_map.t }

let empty = { leaves = Int_map.empty }

let pp f ledger =
  Format.fprintf f "Ledger { leaves: %a }"
    (pp_map Causet.Debug.int pp_utxo)
    (Int_map.bindings ledger.leaves)

(* A leaf's siblings, from the leaf up. *)
type proof = Proof of int64 list [@@unboxed]

let pp_proof f (Proof siblings) =
  Format.fprintf f "Proof(%a)"
    (Causet.Debug.list (fun f h -> Format.fprintf f "%Lu" h))
    siblings

let mix x =
  let open Int64 in
  let x = logxor x (shift_right_logical x 30) in
  let x = mul x 0xbf58_476d_1ce4_e5b9L in
  let x = logxor x (shift_right_logical x 27) in
  let x = mul x 0x94d0_49bb_1331_11ebL in
  logxor x (shift_right_logical x 31)

let leaf_hash index leaf =
  let leaf =
    match leaf with None -> 0x5bd1_e995L | Some (Commitment c) -> Int64.logor (mix c) 1L
  in
  mix (Int64.logxor (Int64.mul (Int64.of_int index) 0x9e37_79b9_7f4a_7c15L) leaf)

let node_hash left right =
  mix (Int64.logxor left (mix (Int64.add right 0x9e37_79b9_7f4a_7c15L)))

(* Every level, the leaves' hashes first and the root last. *)
let levels ledger =
  let level =
    Array.init leaves (fun index ->
        leaf_hash index (Option.map commitment (Int_map.find_opt index ledger.leaves)))
  in
  let rec up level acc =
    if Array.length level = 1 then List.rev (level :: acc)
    else
      let next =
        Array.init
          (Array.length level / 2)
          (fun i -> node_hash level.(2 * i) level.((2 * i) + 1))
      in
      up next (level :: acc)
  in
  Array.of_list (up level [])

let root ledger = Root (levels ledger).(depth).(0)

(* The proof of the leaf at [index], present or empty. *)
let prove ledger index =
  let levels = levels ledger in
  let rec go level position acc =
    if level = depth then List.rev acc
    else go (level + 1) (position / 2) (levels.(level).(position lxor 1) :: acc)
  in
  Proof (go 0 index [])

(* The UTXOs of [owner], by leaf index. *)
let owned ledger owner =
  Int_map.bindings ledger.leaves
  |> List.filter (fun (_, (utxo : utxo)) -> utxo.recipient = Key owner)

(* A free leaf, the lowest. *)
let free_leaf ledger =
  let rec go index =
    if index >= leaves then None
    else if Int_map.mem index ledger.leaves then go (index + 1)
    else Some index
  in
  go 0

(* Whether [proof] shows [leaf] (a commitment, or empty) at [index] under
   [root]. *)
let verify (Root root) index leaf (Proof siblings) =
  if index < 0 || index >= leaves || List.length siblings <> depth then false
  else
    let hash, _ =
      List.fold_left
        (fun (hash, position) sibling ->
          let hash =
            if position mod 2 = 0 then node_hash hash sibling else node_hash sibling hash
          in
          (hash, position / 2))
        (leaf_hash index leaf, index)
        siblings
    in
    hash = root

(* ------------------------------------------------------------------------ *)
(* What a light node asks a full peer, and what it answers. *)

(* A leaf the light node expects to hold [expected]. *)
type index_ref = { index : int; expected : commitment } [@@deriving debug]

(* Some leaves of the ledger at [root], each with its proof. *)
type probe_request = { root : root; items : index_ref list } [@@deriving debug]

(* One leaf as the peer answers it: the UTXO there, if any, and its proof. *)
type proved_item = { index : int; utxo : utxo option; proof : proof } [@@deriving debug]
type probe_response = { root : root; items : proved_item list } [@@deriving debug]

(* A peer's answer to a probe. *)
type peer_answer =
  | Data of probe_response
  | Unavailable  (** The peer does not have the ledger at that root. *)
[@@deriving debug]

(* What a verified probe shows at its root. *)
type verified = {
  root : root;
  found : (int * utxo) list;  (** The leaves that hold the expected UTXO. *)
  missing : int list;
      (** The leaves that do not: the expected UTXO is not in the ledger. *)
}
[@@deriving debug]

(* Whether the response answers exactly the request: its root, and every
   requested leaf once, nothing else. Checked before verification. *)
let covers (request : probe_request) (response : probe_response) =
  if response.root <> request.root then
    Error
      (Format.asprintf "answered root %a, asked %a" pp_root response.root pp_root
         request.root)
  else
    let asked =
      List.fold_left
        (fun s (item : index_ref) -> Int_set.add item.index s)
        Int_set.empty request.items
    in
    let rec answer seen = function
      | [] ->
          if Int_set.equal seen asked then Ok ()
          else Error "answered other leaves than asked"
      | (item : proved_item) :: rest ->
          if Int_set.mem item.index seen then
            Error (Printf.sprintf "leaf %d answered twice" item.index)
          else answer (Int_set.add item.index seen) rest
    in
    answer Int_set.empty response.items

(* Verifies every item's proof, and says which expected UTXOs are there. *)
let verify_response (request : probe_request) (response : probe_response) =
  let expected =
    List.fold_left
      (fun m (item : index_ref) -> Int_map.add item.index item.expected m)
      Int_map.empty request.items
  in
  let rec go found missing = function
    | [] ->
        Ok { root = response.root; found = List.rev found; missing = List.rev missing }
    | (item : proved_item) :: rest -> (
        let leaf = Option.map commitment item.utxo in
        if not (verify response.root item.index leaf item.proof) then
          Error (Printf.sprintf "the proof of leaf %d fails" item.index)
        else
          match item.utxo with
          | Some utxo when Some (commitment utxo) = Int_map.find_opt item.index expected
            ->
              go ((item.index, utxo) :: found) missing rest
          | _ -> go found (item.index :: missing) rest)
  in
  go [] [] response.items
