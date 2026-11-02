(* The part of the mempool wallet send reads: which commitments its
   transactions spend, and a bounded, cursor-pinned scan of one owner's
   unspent outputs. *)

open Domain

(* A stable cursor for one owner's scan, valid for one mempool generation at
   one chain tip. *)
type owned_outputs_cursor = {
  owner : owner;
  generation : int;
  tip : block_hash;
  after_exclusive : commitment option;
}
[@@deriving debug]

let cursor_owner c = c.owner

type owned_outputs_chunk = {
  items : (commitment * utxo) list;
  next : owned_outputs_cursor option;
  scanned_positions : int;
}
[@@deriving debug]

type owned_outputs_read_error = Invalid_limits | Stale [@@deriving debug]

type t = {
  generation : int;  (** Bumped on every structural change and every new tip. *)
  tip : block_hash;
  tip_root : root;  (** The UTXO root the mempool is built on. *)
  spends : Commitment_set.t;
  outputs : utxo Commitment_map.t;
  owner_outputs : Commitment_set.t Owner_map.t;
}

let create tip tip_root =
  {
    generation = 0;
    tip;
    tip_root;
    spends = Commitment_set.empty;
    outputs = Commitment_map.empty;
    owner_outputs = Owner_map.empty;
  }

let tip_root m = m.tip_root
let with_tip_root m tip_root = { m with tip_root }

(* Accepts a transaction: its inputs are spent, its outputs indexed. *)
let accept m inputs outputs =
  let spends = List.fold_left (fun s c -> Commitment_set.add c s) m.spends inputs in
  let m =
    List.fold_left
      (fun m utxo ->
        let c = commitment utxo in
        let owner_outputs =
          match utxo.recipient with
          | Key owner ->
              let owned =
                Option.value
                  (Owner_map.find_opt owner m.owner_outputs)
                  ~default:Commitment_set.empty
              in
              Owner_map.add owner (Commitment_set.add c owned) m.owner_outputs
          | Script -> m.owner_outputs
        in
        { m with owner_outputs; outputs = Commitment_map.add c utxo m.outputs })
      { m with spends } outputs
  in
  { m with generation = m.generation + 1 }

(* A submitted transaction enters the mempool. *)
let submit m (tx : submitted_tx) = accept m tx.inputs tx.outputs

(* A new chain tip. *)
let set_tip m tip = { m with tip; generation = m.generation + 1 }
let has_spend m c = Commitment_set.mem c m.spends

let begin_owned_scan m owner =
  { owner; generation = m.generation; tip = m.tip; after_exclusive = None }

let validate_cursor m (cursor : owned_outputs_cursor) =
  if cursor.generation = m.generation && cursor.tip = m.tip then Ok () else Error Stale

(* One bounded page of the owner's unspent outputs. The cursor advances over
   every inspected position, spent ones included. *)
let owned_outputs_chunk m (cursor : owned_outputs_cursor) max_scan_positions max_matches =
  if max_scan_positions = 0 || max_matches = 0 then Error Invalid_limits
  else
    match validate_cursor m cursor with
    | Error e -> Error e
    | Ok () -> (
        match Owner_map.find_opt cursor.owner m.owner_outputs with
        | None -> Ok { items = []; next = None; scanned_positions = 0 }
        | Some owned ->
            let positions =
              match cursor.after_exclusive with
              | None -> Commitment_set.to_seq owned
              | Some after ->
                  Commitment_set.to_seq_from after owned
                  |> Seq.drop_while (fun c -> Commitment_ord.compare c after = 0)
            in
            let rec scan positions cursor items scanned =
              if scanned >= max_scan_positions || List.length items >= max_matches then
                (positions, cursor, items, scanned)
              else
                match positions () with
                | Seq.Nil -> (Seq.empty, cursor, items, scanned)
                | Seq.Cons (c, rest) ->
                    let cursor = { cursor with after_exclusive = Some c } in
                    if has_spend m c then scan rest cursor items (scanned + 1)
                    else
                      scan rest cursor
                        ((c, Commitment_map.find c m.outputs) :: items)
                        (scanned + 1)
            in
            let rest, cursor, items, scanned = scan positions cursor [] 0 in
            let next = if Seq.is_empty rest then None else Some cursor in
            Ok { items = List.rev items; next; scanned_positions = scanned })
