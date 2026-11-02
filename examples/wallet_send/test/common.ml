(* A wallet-send node under test, its Services recording what they are
   asked. The Rust port's examples/wallet-send/tests/common/mod.rs. *)

open Causet
open Wallet_send
open Domain
open State
module Harness = Causet_test.Node
module L = Partial_ledger
module P = Peers

(* What the Services were asked, in order. *)
type asked = Asked_collect of collect_request | Asked_prove of prove_request

(* A recorded request with the route its answer goes back on. *)
type ('req, 'ev) pending = { request : 'req; reply : 'ev reply }

(* A wallet-cache write: [(rpc, query, root, found, missing)]. *)
type cache_write = rpc_id * Request_id.t * root * int list * int list

(* A partial-store write: [(rpc, query, root, proved UTXOs by leaf)]. *)
type partial_write = rpc_id * Request_id.t * root * (int * utxo) list

type verify_ask = {
  job : L.job_id;
  peer : P.peer_id;
  request_id : Request_id.t;
  probe : Ledger.probe_request;
  response : Ledger.probe_response;
}

type record = {
  mutable asked : asked list;
  mutable collects : (collect_request, Node.collected) pending list;
  mutable proves : (prove_request, Node.proved) pending list;
  mutable sends :
    (P.peer_id * Request_id.t * Ledger.probe_request, P.wire_answer) pending list;
      (** Probes sent to peers: [(peer, request ID, request)]. *)
  mutable verifies : (verify_ask, L.verify_done) pending list;
  mutable cache_stores : (cache_write, L.stored) pending list;
  mutable partial_stores : (partial_write, L.stored) pending list;
  mutable responses : (rpc_id * send_response) list;
  mutable scans : rpc_id list;  (** Which RPC each mempool scan was for. *)
}

let new_record () =
  {
    asked = [];
    collects = [];
    proves = [];
    sends = [];
    verifies = [];
    cache_stores = [];
    partial_stores = [];
    responses = [];
    scans = [];
  }

let services record timer : Node.services =
  {
    collect =
      (fun request reply ->
        record.asked <- record.asked @ [ Asked_collect request ];
        record.collects <- record.collects @ [ { request; reply } ]);
    prove =
      (fun request reply ->
        record.asked <- record.asked @ [ Asked_prove request ];
        record.proves <- record.proves @ [ { request; reply } ]);
    respond =
      (fun rpc_id response ->
        record.responses <- record.responses @ [ (rpc_id, response) ]);
    send =
      (fun peer request_id request reply ->
        record.sends <-
          record.sends @ [ { request = (peer, request_id, request); reply } ]);
    verify =
      (fun job peer request_id probe response reply ->
        record.verifies <-
          record.verifies
          @ [ { request = { job; peer; request_id; probe; response }; reply } ]);
    store_cache =
      (fun rpc_id query root found missing reply ->
        record.cache_stores <-
          record.cache_stores
          @ [ { request = (rpc_id, query, root, found, missing); reply } ]);
    store_partial =
      (fun rpc_id query root utxos reply ->
        record.partial_stores <-
          record.partial_stores @ [ { request = (rpc_id, query, root, utxos); reply } ]);
    timer;
  }

(* Records the RPC of every mempool scan, read from the Action itself. *)
let scans record =
  {
    Observe.none with
    action_start =
      (fun _ payload ->
        match Message.get Node.action_message payload with
        | Some (Scan_mempool { request }) ->
            record.scans <- record.scans @ [ request.rpc_id ]
        | _ -> ());
  }

type wallet_node = (Node.state, Node.services) Harness.t

type fixture = {
  node : wallet_node;
  mount : (Node.state, Node.services, Node.action, Node.effect_, never) mount;
  ledger : (L.state, L.services, L.action, L.effect_, L.output) mount;
  peers : (P.state, P.services, P.action, P.effect_, P.output) mount;
  record : record;
  clock : Clock.Manual.m;  (** The node's clock, which its simulated timer runs on. *)
}

let tip = Block_hash 1L
let root = Root 100L
let rpc_id = Rpc_id 1
let second_rpc_id = Rpc_id 2
let owner = Owner 1
let second_owner = Owner 2

let request_for owner : send_request =
  { from = owner; to_ = owner; amount = 10; fee = Some 0; memo = "" }

let request = request_for owner

(* A node with [rpc_id] pending. *)
let state_with_pending_request () : Node.state =
  let state = Node.create tip root in
  {
    state with
    rpc = Rpc_map.add rpc_id { Node.req = request; status = Pending } state.rpc;
  }

let fixture (state : Node.state) =
  let record = new_record () in
  let clock = Clock.Manual.create 1_000_000_000 in
  let timer, control = Timer.simulated clock in
  let node =
    Harness.create ~name:"node" Node.component ~state ~services:(services record timer)
      ~session:1 clock
  in
  let node =
    Harness.check (Harness.with_timer node control) "wallet send" Node.invariants
  in
  Store.add_observer (Harness.store node) (scans record);
  let at = Path.root Node.component in
  {
    node;
    mount = Harness.mount node at;
    ledger = Harness.mount node (Path.child at Node.ledger_slot);
    peers = Harness.mount node (Path.child at Node.peers_slot);
    record;
    clock;
  }

let st f = Harness.state f.node

let wallet_utxo_for owner amount nonce =
  let utxo = base owner amount nonce in
  (commitment utxo, utxo)

let wallet_utxo amount nonce = wallet_utxo_for owner amount nonce

(* The Collect request asked at position [index] of [asked]. *)
let collect_request asked index =
  match List.nth_opt asked index with
  | Some (Asked_collect request) -> request
  | _ -> Alcotest.failf "expected a collect at %d" index

(* A persisted page answering [request]. *)
let page (request : collect_request) utxos frontier_exclusive next_index complete =
  Ok
    {
      scanned_positions = max 0 (next_index - request.next_index);
      utxos;
      frontier_exclusive;
      next_index;
      complete;
    }

(* A ledger at [root]'s place: the owner's UTXOs of [amounts], at leaves 0,
   1, ... *)
let ledger_of amounts =
  {
    Ledger.leaves =
      List.fold_left
        (fun (leaves, index) amount ->
          (Int_map.add index (base owner amount index) leaves, index + 1))
        (Int_map.empty, 0) amounts
      |> fst;
  }

let dispatch f action = Harness.dispatch f.node f.mount action

(* The chain moves to [ledger]'s root. *)
let move_tip f ledger =
  let (Root r as root) = Ledger.root ledger in
  ignore (dispatch f (New_tip { tip = Block_hash r; root }))

(* The wallet learns of every UTXO of [ledger], and is seeded. *)
let learn f ledger =
  let entries =
    List.map
      (fun (index, (utxo : utxo)) -> (index, commitment utxo, utxo.amount))
      (Ledger.owned ledger owner)
  in
  ignore (Harness.dispatch f.node f.ledger (Wallet_seen { owner; entries }));
  ignore (Harness.dispatch f.node f.ledger Seeded)

(* [peer] connects, serving [root]. *)
let connect f peer root =
  ignore (Harness.dispatch f.node f.peers (Connected { peer = Peer_id peer; root }))

let last list = List.nth list (List.length list - 1)

(* Starts the send and lets its collection find nothing stored, so it asks
   for hydration. *)
let reach_hydration f =
  ignore (dispatch f (Start { rpc_id }));
  let request = (last f.record.collects).request in
  let result = page request [] 0 0 true in
  ignore (dispatch f (Persisted_page { request; result }))

(* Delivers what the node holds, until nothing is. *)
let deliver_all f =
  while Harness.deliver_next f.node <> None do
    ()
  done

(* Takes the oldest of a recorded list. *)
let take_first list =
  match list with x :: rest -> (x, rest) | [] -> Alcotest.fail "nothing recorded"

let outcome =
  Alcotest.testable
    (fun f o ->
      Format.pp_print_string f
        (match o with
        | Executed -> "Executed"
        | Disabled -> "Disabled"
        | Faulted -> "Faulted"))
    ( = )

let response = Alcotest.testable pp_send_response ( = )
let responses = Alcotest.(list (pair (testable pp_rpc_id ( = )) response))
let error_of (r : send_response) = Option.value r.error ~default:""

let contains s sub =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0
