(* A full peer: a node of its own, holding the whole ledger at the last few
   roots it caught up to, and answering probes for leaves with their proofs.
   A faulty peer is the same machine with a fault: it lies in one way on
   every answer, or stays silent. *)

open Causet
open Domain
open Ledger
open Peers

(* How many recent roots a peer serves. *)
let kept_roots = 3

(* How a peer answers. *)
type fault =
  | Honest
  | Alter_item  (** One item's UTXO is altered (its proof is for the original). *)
  | Drop_item  (** One item is left out. *)
  | Duplicate_item  (** One item is answered twice. *)
  | Extra_item  (** A leaf nobody asked for is added, with a valid proof. *)
  | Other_root  (** The answer is for the peer's newest root, whatever was asked. *)
  | Silent  (** No answer. *)
[@@deriving debug]

type state = {
  id : peer_id;
  fault : fault;
  ledgers : (root * Ledger.t) list;  (** The ledgers it serves, oldest first. *)
  answered : probe_request Request_map.t;
      (** Every request it answered, by the light node's request ID. *)
}

let create id fault = { id; fault; ledgers = []; answered = Request_map.empty }

let newest_root state =
  match List.rev state.ledgers with (root, _) :: _ -> Some root | [] -> None

let ledger state root = List.assoc_opt root state.ledgers

(* A probe from the light node, through the network. *)
type incoming = { request_id : Request_id.t; request : probe_request }
[@@deriving message ~name:"peer.incoming"]

type services = {
  answer : peer_id -> Request_id.t -> peer_answer -> unit;
  announce : peer_id -> root -> unit;
}

type action =
  | Caught_up of { ledger : Ledger.t }  (** The peer caught up to a new block's ledger. *)
  | Probe of { request_id : Request_id.t; request : probe_request }
[@@deriving message ~name:"peer.action"]

type effect_ =
  | Answer of { request_id : Request_id.t; answer : peer_answer }
  | Announce of { root : root }
[@@deriving message ~name:"peer.effect"]

(* The peer's answer, with its fault applied. *)
let answer state (request : probe_request) =
  let root =
    match state.fault with Other_root -> newest_root state | _ -> Some request.root
  in
  match Option.bind root (ledger state) with
  | None -> Unavailable
  | Some ledger ->
      let root = Option.get root in
      let item index =
        { index; utxo = Int_map.find_opt index ledger.leaves; proof = prove ledger index }
      in
      let items = List.map (fun (i : index_ref) -> item i.index) request.items in
      let items =
        match state.fault with
        | Alter_item -> (
            match items with
            | first :: rest ->
                let utxo = Option.value first.utxo ~default:(base (Owner 0) 1 0) in
                { first with utxo = Some { utxo with amount = utxo.amount + 1_000 } }
                :: rest
            | [] -> [])
        | Drop_item -> (
            match List.rev items with _ :: rest -> List.rev rest | [] -> [])
        | Duplicate_item -> (
            match items with first :: _ -> items @ [ first ] | [] -> [])
        | Extra_item -> (
            let asked index =
              List.exists (fun (i : index_ref) -> i.index = index) request.items
            in
            match
              List.find_opt
                (fun (index, _) -> not (asked index))
                (Int_map.bindings ledger.leaves)
            with
            | Some (index, _) -> items @ [ item index ]
            | None -> items)
        | Honest | Other_root | Silent -> items
      in
      Data { root; items }

type ctx = (state, services, action, effect_, never) Ctx.t

let reduce (ctx : ctx) (action : action) _ =
  let state = Ctx.state ctx in
  match action with
  | Caught_up { ledger } ->
      let root = root ledger in
      if newest_root state <> Some root then (
        let ledgers = state.ledgers @ [ (root, ledger) ] in
        let ledgers =
          if List.length ledgers > kept_roots then List.tl ledgers else ledgers
        in
        Ctx.set_state ctx { state with ledgers };
        Ctx.emit_effect ctx (Announce { root }))
  | Probe { request_id; request } ->
      if state.fault <> Silent then (
        let state =
          { state with answered = Request_map.add request_id request state.answered }
        in
        Ctx.set_state ctx state;
        Ctx.emit_effect ctx (Answer { request_id; answer = answer state request }))

let handle ctx (eff : effect_) _ =
  match Effect_ctx.state ctx with
  | None -> ()
  | Some state -> (
      let service : services = Effect_ctx.service ctx in
      match eff with
      | Answer { request_id; answer } -> service.answer state.id request_id answer
      | Announce { root } -> service.announce state.id root)

let component : (state, services, action, effect_, never) Component.t =
  Component.make ~name:"full_peer" ~actions:action_message ~effects:effect_message
    ~outputs:Message.never ~reduce ~handle
    ~register:(fun r ->
      Registrar.event r incoming_message (fun (event : incoming) port ->
          Port.emit_action port
            (Probe { request_id = event.request_id; request = event.request })))
    ()
