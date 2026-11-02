(* Wallet send's lifecycle State and its transitions. A request ID is
   causet's [Request_id.t], made by the reducer ([Ctx.new_request]) rather
   than a counter kept in State, so it also tells two runs of the node
   apart. The State is a value: every transition returns the next one. *)

open Causet
open Domain
open Mempool

let owner_max_scan_positions = 4_096
let owner_max_matches = 32
let mempool_max_scan_positions = 32
let mempool_max_matches = 32

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

type collection_source =
  | Persisted of { chunk_seq : int; next_index : int }
  | Mempool_source of { chunk_seq : int }

(* Selects base-currency inputs incrementally: the largest candidates, in
   stable descending order, at most [max_inputs] of them. *)
type input_selector = {
  required : int;
  eligible_total : int;
  candidates : (commitment * utxo) list;
}

type collection = {
  request_id : Request_id.t;
  anchor_root : root;
  timestamp_ms : int;
  req : send_request;
  selector : input_selector;
  mempool_cursor : owned_outputs_cursor;
  source : collection_source;
}

let new_selector amount fee =
  if amount = 0 then Error "amount must be greater than zero"
  else
    match checked_add amount fee with
    | None -> Error "amount+fee overflow"
    | Some required -> Ok { required; eligible_total = 0; candidates = [] }

let consider selector commitment (utxo : utxo) =
  match utxo.recipient with
  | Script -> Ok selector
  | Key _ -> (
      match ensure_key_utxo_nonzero_amount utxo with
      | Error err ->
          Error
            (Format.asprintf "owned utxo %a violates spendable single-asset invariant: %s"
               pp_commitment commitment err)
      | Ok () ->
          if utxo.token <> base_token || utxo.data <> 0 then Ok selector
          else
            let eligible_total = saturating_add selector.eligible_total utxo.amount in
            (* The first position whose amount is below this one's. *)
            let rec insert_at i = function
              | (_, (c : utxo)) :: rest when c.amount >= utxo.amount ->
                  insert_at (i + 1) rest
              | _ -> i
            in
            let at = insert_at 0 selector.candidates in
            if at >= max_inputs then Ok { selector with eligible_total }
            else
              let rec insert i = function
                | rest when i = at -> (commitment, utxo) :: rest
                | x :: rest -> x :: insert (i + 1) rest
                | [] -> [ (commitment, utxo) ]
              in
              let candidates = insert 0 selector.candidates in
              let candidates = List.filteri (fun i _ -> i < max_inputs) candidates in
              Ok { selector with eligible_total; candidates })

let finish selector =
  let rec go total = function
    | [] -> None
    | (_, (utxo : utxo)) :: rest -> (
        match checked_add total utxo.amount with
        | None -> Some (Error "base funds overflow when summing inputs")
        | Some total -> if total >= selector.required then Some (Ok ()) else go total rest
        )
  in
  match go 0 selector.candidates with
  | Some (Ok ()) -> Ok selector.candidates
  | Some (Error e) -> Error e
  | None ->
      if selector.eligible_total < selector.required then
        Error
          (Printf.sprintf "insufficient base funds (have=%d, need=%d)"
             selector.eligible_total selector.required)
      else
        Error
          (Printf.sprintf "transfer requires more than %d inputs but hard max is %d"
             max_inputs max_inputs)

type phase = Collecting of collection | Proving of { request_id : Request_id.t }

type persisted_transition =
  | Collect of collect_request
  | Scan_mempool of mempool_request
  | Hydrate of { anchor_root : root; selection_error : string }

type mempool_transition =
  | Continue
  | Prove of prove_request
  | Mempool_hydrate of { anchor_root : root; selection_error : string }

(* Deterministic correlation for the asynchronous wallet-send lifecycle. *)
type t = {
  pending : phase Rpc_map.t;
  owner_by_rpc : owner Rpc_map.t;
  rpc_by_owner : rpc_id Owner_map.t;
  mempool_scan_ready : Rpc_set.t;
  last_mempool_scan_rpc : rpc_id option;
}

let empty =
  {
    pending = Rpc_map.empty;
    owner_by_rpc = Rpc_map.empty;
    rpc_by_owner = Owner_map.empty;
    mempool_scan_ready = Rpc_set.empty;
    last_mempool_scan_rpc = None;
  }

let has_work s rpc_id = Rpc_map.mem rpc_id s.pending || Rpc_map.mem rpc_id s.owner_by_rpc

let owner_is_available s rpc_id owner =
  match Owner_map.find_opt owner s.rpc_by_owner with
  | None -> true
  | Some reserved -> reserved = rpc_id

let reserve_owner s rpc_id owner =
  if not (owner_is_available s rpc_id owner) then None
  else
    let rpc_by_owner =
      match Rpc_map.find_opt rpc_id s.owner_by_rpc with
      | Some previous when previous <> owner -> Owner_map.remove previous s.rpc_by_owner
      | _ -> s.rpc_by_owner
    in
    Some
      {
        s with
        owner_by_rpc = Rpc_map.add rpc_id owner s.owner_by_rpc;
        rpc_by_owner = Owner_map.add owner rpc_id rpc_by_owner;
      }

let release_owner s rpc_id =
  match Rpc_map.find_opt rpc_id s.owner_by_rpc with
  | None -> s
  | Some owner ->
      let owner_by_rpc = Rpc_map.remove rpc_id s.owner_by_rpc in
      let rpc_by_owner =
        if Owner_map.find_opt owner s.rpc_by_owner = Some rpc_id then
          Owner_map.remove owner s.rpc_by_owner
        else s.rpc_by_owner
      in
      { s with owner_by_rpc; rpc_by_owner }

let can_start s rpc_id = not (Rpc_map.mem rpc_id s.pending)

let not_ready s rpc_id =
  { s with mempool_scan_ready = Rpc_set.remove rpc_id s.mempool_scan_ready }

let persisted_request rpc_id (collection : collection) : collect_request option =
  match collection.source with
  | Persisted { chunk_seq; next_index } ->
      Some
        {
          request_id = collection.request_id;
          rpc_id;
          chunk_seq;
          anchor_root = collection.anchor_root;
          owner = collection.req.from;
          next_index;
          max_scan_positions = owner_max_scan_positions;
          max_matches = owner_max_matches;
          mempool_cursor = collection.mempool_cursor;
        }
  | Mempool_source _ -> None

let mempool_request rpc_id (collection : collection) : mempool_request option =
  match collection.source with
  | Mempool_source { chunk_seq } ->
      Some
        {
          request_id = collection.request_id;
          rpc_id;
          chunk_seq;
          anchor_root = collection.anchor_root;
          cursor = collection.mempool_cursor;
          max_scan_positions = mempool_max_scan_positions;
          max_matches = mempool_max_matches;
        }
  | Persisted _ -> None

let begin_collecting s request_id rpc_id anchor_root timestamp_ms (req : send_request)
    mempool_cursor =
  let s = not_ready s rpc_id in
  if cursor_owner mempool_cursor <> req.from then
    Error "wallet-send mempool cursor owner mismatch"
  else
    match new_selector req.amount (Option.value req.fee ~default:0) with
    | Error e -> Error e
    | Ok selector -> (
        let collection =
          {
            request_id;
            anchor_root;
            timestamp_ms;
            req;
            selector;
            mempool_cursor;
            source = Persisted { chunk_seq = 0; next_index = 0 };
          }
        in
        match persisted_request rpc_id collection with
        | None -> Error "wallet-send persisted collection phase missing"
        | Some request ->
            Ok
              ( { s with pending = Rpc_map.add rpc_id (Collecting collection) s.pending },
                request ))

let is_collecting_persisted s (request : collect_request) =
  match Rpc_map.find_opt request.rpc_id s.pending with
  | Some (Collecting ({ source = Persisted _; _ } as collection)) ->
      persisted_request request.rpc_id collection = Some request
  | _ -> false

let is_collecting_mempool s (request : mempool_request) =
  match Rpc_map.find_opt request.rpc_id s.pending with
  | Some (Collecting ({ source = Mempool_source _; _ } as collection)) ->
      mempool_request request.rpc_id collection = Some request
  | _ -> false

let collection s rpc_id request_id =
  match Rpc_map.find_opt rpc_id s.pending with
  | Some (Collecting collection) when collection.request_id = request_id ->
      Some collection
  | _ -> None

let collection_matches_rpc_request s rpc_id request_id current =
  match collection s rpc_id request_id with
  | Some collection -> collection.req = current
  | None -> false

let collection_pins s rpc_id request_id =
  Option.map
    (fun (c : collection) -> (c.anchor_root, c.mempool_cursor))
    (collection s rpc_id request_id)

let next_mempool_scan_rpc s =
  let after_last =
    Option.bind s.last_mempool_scan_rpc (fun last ->
        Rpc_set.find_first_opt (fun r -> Rpc_ord.compare r last > 0) s.mempool_scan_ready)
  in
  match after_last with
  | Some r -> Some r
  | None -> Rpc_set.min_elt_opt s.mempool_scan_ready

(* The next owner whose mempool scan continues, round robin. [Error] when
   readiness names an RPC that is not collecting from the mempool. *)
let next_mempool_scan_request s =
  match next_mempool_scan_rpc s with
  | None -> Ok None
  | Some rpc_id -> (
      match Rpc_map.find_opt rpc_id s.pending with
      | Some (Collecting collection) -> (
          match mempool_request rpc_id collection with
          | Some request -> Ok (Some request)
          | None -> Error rpc_id)
      | _ -> Error rpc_id)

let validate_persisted_chunk (request : collect_request) (chunk : owned_utxos_chunk) =
  let advanced = max 0 (chunk.next_index - request.next_index) in
  let n = List.length chunk.utxos in
  let bounds_valid =
    n <= request.max_matches
    && chunk.scanned_positions <= request.max_scan_positions
    && n <= chunk.scanned_positions
    && chunk.next_index >= request.next_index
    && chunk.next_index <= chunk.frontier_exclusive
    && advanced = chunk.scanned_positions
    && chunk.complete = (chunk.next_index = chunk.frontier_exclusive)
    && (chunk.complete || chunk.next_index > request.next_index)
  in
  if bounds_valid then Ok ()
  else Error "invalid wallet-send persisted UTXO chunk bounds or cursor progress"

(* The State after a persisted page, and what follows; [None], the State
   unchanged, when the page is not for the current collection. *)
let apply_persisted_chunk s (request : collect_request) result spent =
  match Rpc_map.find_opt request.rpc_id s.pending with
  | Some (Collecting collection)
    when persisted_request request.rpc_id collection = Some request -> (
      let rpc_id = request.rpc_id in
      (* The collection leaves [pending]; it goes back in when it goes on. *)
      let s = { s with pending = Rpc_map.remove rpc_id s.pending } in
      let hydrate s (collection : collection) selection_error =
        Some
          ( not_ready s rpc_id,
            Hydrate { anchor_root = collection.anchor_root; selection_error } )
      in
      let to_mempool s collection =
        let collection =
          {
            collection with
            source = Mempool_source { chunk_seq = request.chunk_seq + 1 };
          }
        in
        let s = { s with mempool_scan_ready = Rpc_set.add rpc_id s.mempool_scan_ready } in
        match mempool_request rpc_id collection with
        | None -> hydrate s collection "wallet-send mempool collection phase missing"
        | Some next ->
            Some
              ( { s with pending = Rpc_map.add rpc_id (Collecting collection) s.pending },
                Scan_mempool next )
      in
      match result with
      | Error _ ->
          (* A failed read: go on with the candidates so far, from the
             mempool. *)
          to_mempool s collection
      | Ok chunk -> (
          match validate_persisted_chunk request chunk with
          | Error error -> hydrate s collection error
          | Ok () -> (
              let rec select selector = function
                | [] -> Ok selector
                | (c, utxo) :: rest -> (
                    if Commitment_set.mem c spent then select selector rest
                    else
                      match consider selector c utxo with
                      | Ok selector -> select selector rest
                      | Error e -> Error e)
              in
              match select collection.selector chunk.utxos with
              | Error selection_error -> hydrate s collection selection_error
              | Ok selector ->
                  let collection = { collection with selector } in
                  if not chunk.complete then
                    let collection =
                      {
                        collection with
                        source =
                          Persisted
                            {
                              chunk_seq = request.chunk_seq + 1;
                              next_index = chunk.next_index;
                            };
                      }
                    in
                    match persisted_request rpc_id collection with
                    | None ->
                        Some
                          ( s,
                            Hydrate
                              {
                                anchor_root = collection.anchor_root;
                                selection_error =
                                  "wallet-send persisted collection phase missing";
                              } )
                    | Some next ->
                        Some
                          ( {
                              s with
                              pending =
                                Rpc_map.add rpc_id (Collecting collection) s.pending;
                            },
                            Collect next )
                  else to_mempool s collection)))
  | _ -> None

(* The State after a mempool page, and what follows; [None], the State
   unchanged, when the page is not for the current collection. *)
let apply_mempool_chunk s (request : mempool_request) (chunk : owned_outputs_chunk) =
  match Rpc_map.find_opt request.rpc_id s.pending with
  | Some (Collecting collection)
    when mempool_request request.rpc_id collection = Some request -> (
      let rpc_id = request.rpc_id in
      let s = { s with pending = Rpc_map.remove rpc_id s.pending } in
      let hydrate s selection_error =
        Some
          ( not_ready s rpc_id,
            Mempool_hydrate { anchor_root = collection.anchor_root; selection_error } )
      in
      let n = List.length chunk.items in
      if
        n > request.max_matches
        || chunk.scanned_positions > request.max_scan_positions
        || n > chunk.scanned_positions
      then hydrate s "invalid wallet-send mempool UTXO chunk bounds"
      else
        let s = { s with last_mempool_scan_rpc = Some rpc_id } in
        let rec select selector = function
          | [] -> Ok selector
          | (c, utxo) :: rest -> (
              match consider selector c utxo with
              | Ok selector -> select selector rest
              | Error e -> Error e)
        in
        match select collection.selector chunk.items with
        | Error selection_error -> hydrate s selection_error
        | Ok selector -> (
            let collection = { collection with selector } in
            match chunk.next with
            | Some next_cursor ->
                if
                  next_cursor = collection.mempool_cursor
                  || cursor_owner next_cursor <> collection.req.from
                then hydrate s "invalid wallet-send mempool UTXO cursor progress"
                else
                  let collection =
                    {
                      collection with
                      mempool_cursor = next_cursor;
                      source = Mempool_source { chunk_seq = request.chunk_seq + 1 };
                    }
                  in
                  Some
                    ( {
                        s with
                        pending = Rpc_map.add rpc_id (Collecting collection) s.pending;
                      },
                      Continue )
            | None -> (
                match finish collection.selector with
                | Error selection_error -> hydrate s selection_error
                | Ok available_utxos ->
                    let prove =
                      {
                        request_id = collection.request_id;
                        rpc_id;
                        anchor_root = collection.anchor_root;
                        timestamp_ms = collection.timestamp_ms;
                        req = collection.req;
                        available_utxos;
                      }
                    in
                    let s =
                      {
                        s with
                        pending =
                          Rpc_map.add rpc_id
                            (Proving { request_id = prove.request_id })
                            s.pending;
                      }
                    in
                    Some (not_ready s rpc_id, Prove prove))))
  | _ -> None

let finish_collecting s rpc_id request_id =
  match collection s rpc_id request_id with
  | None -> None
  | Some _ -> Some (not_ready { s with pending = Rpc_map.remove rpc_id s.pending } rpc_id)

let is_proving s rpc_id request_id =
  match Rpc_map.find_opt rpc_id s.pending with
  | Some (Proving { request_id = actual }) -> actual = request_id
  | _ -> false

let finish_proving s rpc_id request_id =
  if not (is_proving s rpc_id request_id) then None
  else Some (not_ready { s with pending = Rpc_map.remove rpc_id s.pending } rpc_id)

(* The State with the RPC's work gone, and whether it had any. *)
let cancel s rpc_id =
  let had_pending = Rpc_map.mem rpc_id s.pending in
  let had_owner = Rpc_map.mem rpc_id s.owner_by_rpc in
  let s = not_ready { s with pending = Rpc_map.remove rpc_id s.pending } rpc_id in
  (release_owner s rpc_id, had_pending || had_owner)

(* The RPCs with work in progress. *)
let working s = List.map fst (Rpc_map.bindings s.pending)
let pp_opt pp f = Causet.Debug.option pp f

(* The structural invariants the lifecycle relies on: the two reservation
   maps are each other's inverse, so an owner has at most one send; every
   send in progress holds its owner; and only an RPC collecting from the
   mempool is ready for a mempool scan. *)
let invariants s =
  let ( let* ) = Result.bind in
  let rec all f = function
    | [] -> Ok ()
    | x :: rest ->
        let* () = f x in
        all f rest
  in
  let* () =
    all
      (fun (rpc_id, owner) ->
        let reserved = Owner_map.find_opt owner s.rpc_by_owner in
        if reserved = Some rpc_id then Ok ()
        else
          Error
            (Format.asprintf "%a holds %a, which is reserved for %a" pp_rpc_id rpc_id
               pp_owner owner (pp_opt pp_rpc_id) reserved))
      (Rpc_map.bindings s.owner_by_rpc)
  in
  let* () =
    all
      (fun (owner, rpc_id) ->
        let held = Rpc_map.find_opt rpc_id s.owner_by_rpc in
        if held = Some owner then Ok ()
        else
          Error
            (Format.asprintf "%a is reserved for %a, which holds %a" pp_owner owner
               pp_rpc_id rpc_id (pp_opt pp_owner) held))
      (Owner_map.bindings s.rpc_by_owner)
  in
  let* () =
    match
      List.find_opt (fun rpc_id -> not (Rpc_map.mem rpc_id s.owner_by_rpc)) (working s)
    with
    | Some rpc_id ->
        Error
          (Format.asprintf "%a is in progress without its owner reserved" pp_rpc_id rpc_id)
    | None -> Ok ()
  in
  all
    (fun rpc_id ->
      match Rpc_map.find_opt rpc_id s.pending with
      | Some (Collecting { source = Mempool_source _; _ }) -> Ok ()
      | _ ->
          Error
            (Format.asprintf
               "%a is ready for a mempool scan but not collecting from the mempool"
               pp_rpc_id rpc_id))
    (Rpc_set.elements s.mempool_scan_ready)
