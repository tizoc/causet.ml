(* The light node's world, simulated: the chain, the network between the
   light node and its full peers, and the light node's Services (the UTXO
   database over what hydration stored, the prover, the verifier, the two
   stores). Everything answers after seeded delays, on the scenario's clock.
   The Rust port's tests/sim/mod.rs.

   The network keeps each link's messages in order of their links' events,
   loses some messages and delays others past the request timeout, and
   drops what was in flight on a link that went down. *)

open Causet
open Wallet_send
open Domain
open State
module Rng = Causet_test.Rng
module In_flight = Causet_test.In_flight
module L = Partial_ledger
module P = Peers
module Peer_map = P.Peer_map

let ms = 1_000_000

(* A link between the light node and a peer. Its epoch changes whenever it
   goes down or an end restarts, which drops what was in flight on it. *)
type link = {
  mutable up : bool;
  mutable slow : bool;
      (** A slow link takes 3 to 7 s each way, around the request timeout. *)
  mutable epoch : int;
}

(* A partial-store write: [(rpc, query, root, proved UTXOs by leaf)]. *)
type partial_write = rpc_id * Request_id.t * root * (int * utxo) list

type msg =
  | Collected of Node.collected reply * Node.collected
  | Proved of Node.proved reply * Node.proved
  | Verified of L.verify_done reply * L.verify_done
  | Stored of { reply : L.stored reply; event : L.stored; write : partial_write option }
      (** A store's acknowledgement, and for the partial store its write, which becomes
          visible only then. *)
  | To_peer of {
      peer : P.peer_id;
      epoch : int;
      request_id : Request_id.t;
      request : Ledger.probe_request;
    }
  | To_light of {
      peer : P.peer_id;
      epoch : int;
      reply : P.wire_answer reply;
      event : P.wire_answer;
    }
  | Link of P.link_event

module Hydration_key = struct
  type t = rpc_id * Request_id.t

  let compare (a, x) (b, y) =
    match Rpc_ord.compare a b with 0 -> Request_id.compare x y | c -> c
end

module Hydration_map = Map.Make (Hydration_key)
module Hydration_set = Set.Make (Hydration_key)
module Request_set = Set.Make (Request_id)

type peers_mount = (P.state, P.services, P.action, P.effect_, P.output) mount

type peer_mount =
  (Full_peer.state, Full_peer.services, Full_peer.action, Full_peer.effect_, never) mount

type t = {
  rng : Rng.t;
  clock : Clock.Manual.m;
  lossy : bool;
      (** The UTXO database and the prover sometimes never answer, or answer past the
          node's deadline; the verifier sometimes never answers. *)
  chain : (root * Ledger.t) Dynarray.t;
      (** Every ledger the chain has had, oldest first. *)
  mutable partial : utxo Int_map.t Root_map.t;
      (** The proved UTXOs hydration stored, by root: what the UTXO database reads. *)
  mutable writes : int Hydration_map.t;
      (** Partial-store writes in flight, by the send and the hydration they were made
          for. *)
  mutable abandoned : Request_set.t;
      (** Hydrations the test cancelled: their writes may still land, but no send waits
          for them. *)
  mutable failed : Hydration_set.t;
      (** Store writes that failed, by send and hydration. *)
  mutable violations : string list;  (** What the world saw go wrong, newest first. *)
  mutable links : link Peer_map.t;
  mutable roots : root Peer_map.t;  (** The root each peer last announced. *)
  mutable light : (Sink.t * peers_mount) option;
  mutable peers : (Sink.t * peer_mount) Peer_map.t;
  mutable replies : (P.peer_id * int * P.wire_answer reply) Request_map.t;
      (** The light node's requests in flight, with the Reply that answers them. *)
  flight : (msg, P.peer_id) In_flight.t;  (** Link events in order on each peer's link. *)
  mutable nonce : int;
  mutable responses : send_response list Rpc_map.t;
      (** Every answer each RPC got, over every run of the light node, in order. *)
  mutable proved : submitted_tx Int_map.t;
      (** The transactions the prover made, by hash. *)
}

let create rng clock lossy genesis =
  let chain = Dynarray.create () in
  Dynarray.add_last chain (Ledger.root genesis, genesis);
  {
    rng;
    clock;
    lossy;
    chain;
    partial = Root_map.empty;
    writes = Hydration_map.empty;
    abandoned = Request_set.empty;
    failed = Hydration_set.empty;
    violations = [];
    links = Peer_map.empty;
    roots = Peer_map.empty;
    light = None;
    peers = Peer_map.empty;
    replies = Request_map.empty;
    flight = In_flight.create ();
    nonce = 1_000;
    responses = Rpc_map.empty;
    proved = Int_map.empty;
  }

let fresh_nonce sim =
  sim.nonce <- sim.nonce + 1;
  sim.nonce

let newest sim = Dynarray.get_last sim.chain

let ledger_at sim root =
  let rec go i =
    if i < 0 then None
    else
      let at, ledger = Dynarray.get sim.chain i in
      if at = root then Some ledger else go (i - 1)
  in
  go (Dynarray.length sim.chain - 1)

let now sim = Clock.Manual.get sim.clock
let violation sim v = sim.violations <- v :: sim.violations

(* A local Service's answer, 1 to 20 ms from now. *)
let answer_later sim msg =
  let at = now sim + ms + (Rng.below sim.rng 20 * ms) in
  In_flight.send_at sim.flight at msg

(* A store's acknowledgement: one in four from a busy disk, 100 to 300 ms
   from now. *)
let store_later sim msg =
  let busy =
    if Rng.chance sim.rng 1 4 then (100 * ms) + (Rng.below sim.rng 200 * ms) else 0
  in
  let at = now sim + ms + (Rng.below sim.rng 20 * ms) + busy in
  In_flight.send_at sim.flight at msg

(* A lossy Service's answer: one in ten lost, one in ten past the node's
   deadline. *)
let answer_lossy sim msg =
  if not (sim.lossy && Rng.chance sim.rng 1 10) then
    let late = if sim.lossy && Rng.chance sim.rng 1 10 then 40_000 * ms else 0 in
    let at = now sim + ms + (Rng.below sim.rng 20 * ms) + late in
    In_flight.send_at sim.flight at msg

let is_slow sim peer =
  match Peer_map.find_opt peer sim.links with Some link -> link.slow | None -> false

(* A message on [peer]'s link: one in thirty lost, one in thirty past the
   request timeout, and on a slow link every one 3 to 7 s late. *)
let transmit sim peer msg =
  if not (Rng.chance sim.rng 1 30) then
    let late =
      if is_slow sim peer then (3_000 * ms) + (Rng.below sim.rng 4_000 * ms)
      else if Rng.chance sim.rng 1 29 then 6_000 * ms
      else 0
    in
    let at = now sim + ms + (Rng.below sim.rng 20 * ms) + late in
    In_flight.send_at sim.flight at msg

(* A link event, after the link's previous one. *)
let link_event sim peer event =
  let at = now sim + ms + (Rng.below sim.rng 20 * ms) in
  ignore (In_flight.send_in_order sim.flight peer at (Link event))

(* The link goes down, or an end restarted: what was in flight on it is
   lost. *)
let cut sim peer =
  (match Peer_map.find_opt peer sim.links with
  | Some link -> link.epoch <- link.epoch + 1
  | None -> ());
  sim.replies <- Request_map.filter (fun _ (to_, _, _) -> to_ <> peer) sim.replies

let link_up sim peer epoch =
  match Peer_map.find_opt peer sim.links with
  | Some link -> link.up && link.epoch = epoch
  | None -> false

(* ------------------------------------------------------------------------ *)
(* The light node's Services *)

(* Reads the UTXOs hydration stored at the anchor root. A send whose
   hydration is still writing must not collect yet: what it reads would miss
   what that hydration proved. *)
let collect sim (request : collect_request) reply =
  if
    Hydration_map.exists
      (fun (rpc_id, query) _ ->
        rpc_id = request.rpc_id && not (Request_set.mem query sim.abandoned))
      sim.writes
  then
    violation sim
      (Format.asprintf "%a collected while its hydration's write was in flight" pp_rpc_id
         request.rpc_id);
  let owned =
    match Root_map.find_opt request.anchor_root sim.partial with
    | None -> []
    | Some leaves ->
        Int_map.bindings leaves
        |> List.filter (fun (_, (utxo : utxo)) -> utxo.recipient = Key request.owner)
        |> List.map (fun (_, utxo) -> (commitment utxo, utxo))
  in
  let frontier = List.length owned in
  let start = min request.next_index frontier in
  let take =
    min (min request.max_scan_positions request.max_matches) (frontier - start)
  in
  let next_index = start + take in
  let result =
    Ok
      {
        utxos = List.filteri (fun i _ -> i >= start && i < start + take) owned;
        frontier_exclusive = frontier;
        next_index;
        complete = next_index = frontier;
        scanned_positions = take;
      }
  in
  answer_lossy sim (Collected (reply, { request; result }))

let prove sim (request : prove_request) reply =
  let result =
    if Rng.chance sim.rng 1 5 then Error "the prover failed"
    else
      (* The largest inputs first, until they cover the amount and fee. *)
      let need = request.req.amount + Option.value request.req.fee ~default:0 in
      let inputs, total =
        List.fold_left
          (fun (inputs, total) (c, (utxo : utxo)) ->
            if total >= need then (inputs, total) else (c :: inputs, total + utxo.amount))
          ([], 0) request.available_utxos
      in
      let inputs = List.rev inputs in
      let outputs = [ base request.req.to_ request.req.amount (fresh_nonce sim) ] in
      let outputs =
        if total > need then
          outputs @ [ base request.req.from (total - need) (fresh_nonce sim) ]
        else outputs
      in
      let tx = { hash = fresh_nonce sim; inputs; outputs } in
      sim.proved <- Int_map.add tx.hash tx sim.proved;
      Ok tx
  in
  answer_lossy sim
    (Proved (reply, { request_id = request.request_id; rpc_id = request.rpc_id; result }))

(* Records the answer. A send rejected because a store failed must have had
   a write of its own current hydration fail, not one of a hydration
   cancelled since. *)
let respond sim rpc_id (response : send_response) =
  let store_failed =
    match response.error with
    | Some error -> String.starts_with ~prefix:"hydration: storing" error
    | None -> false
  in
  let current_failed =
    Hydration_set.exists
      (fun (rpc, query) -> rpc = rpc_id && not (Request_set.mem query sim.abandoned))
      sim.failed
  in
  if store_failed && not current_failed then
    violation sim
      (Format.asprintf "%a rejected for another hydration's failed write" pp_rpc_id rpc_id);
  let previous = Option.value (Rpc_map.find_opt rpc_id sim.responses) ~default:[] in
  sim.responses <- Rpc_map.add rpc_id (previous @ [ response ]) sim.responses

let send sim peer request_id request reply =
  match Peer_map.find_opt peer sim.links with
  | Some link when link.up ->
      let epoch = link.epoch in
      sim.replies <- Request_map.add request_id (peer, epoch, reply) sim.replies;
      transmit sim peer (To_peer { peer; epoch; request_id; request })
  | _ -> ()

let verify sim job peer request_id request response reply =
  let result = Ledger.verify_response request response in
  let done_ = { L.job; peer; request_id; request; result } in
  (* A lossy verifier loses one answer in ten. *)
  if not (sim.lossy && Rng.chance sim.rng 1 10) then
    answer_later sim (Verified (reply, done_))

let store_cache sim rpc_id query root _ _ reply =
  let ok = not (Rng.chance sim.rng 1 8) in
  if not ok then sim.failed <- Hydration_set.add (rpc_id, query) sim.failed;
  store_later sim
    (Stored { reply; event = Cache { rpc_id; query; root; ok }; write = None })

let store_partial sim rpc_id query root utxos reply =
  let ok = not (Rng.chance sim.rng 1 8) in
  if not ok then sim.failed <- Hydration_set.add (rpc_id, query) sim.failed;
  let key = (rpc_id, query) in
  let n = Option.value (Hydration_map.find_opt key sim.writes) ~default:0 in
  sim.writes <- Hydration_map.add key (n + 1) sim.writes;
  store_later sim
    (Stored
       {
         reply;
         event = Partial { rpc_id; query; root; ok };
         write = Some (rpc_id, query, root, utxos);
       })

(* The light node's Services, its simulated timer on the scenario's
   clock. *)
let light_services sim =
  let timer, control = Timer.simulated sim.clock in
  let services : Node.services =
    {
      collect = collect sim;
      prove = prove sim;
      respond = respond sim;
      send = send sim;
      verify = verify sim;
      store_cache = store_cache sim;
      store_partial = store_partial sim;
      timer;
    }
  in
  (services, control)

(* ------------------------------------------------------------------------ *)
(* A full peer's Service *)

let peer_services sim : Full_peer.services =
  {
    answer =
      (fun peer request_id answer ->
        match Request_map.find_opt request_id sim.replies with
        | None -> ()
        | Some (to_, epoch, reply) ->
            sim.replies <- Request_map.remove request_id sim.replies;
            if to_ = peer && link_up sim peer epoch then
              transmit sim peer
                (To_light { peer; epoch; reply; event = { peer; request_id; answer } }));
    announce =
      (fun peer root ->
        sim.roots <- Peer_map.add peer root sim.roots;
        match Peer_map.find_opt peer sim.links with
        | Some link when link.up -> link_event sim peer (Announced { peer; root })
        | _ -> ());
  }

(* ------------------------------------------------------------------------ *)
(* The world's links *)

(* The light node is (again) at [sink]: the links' traffic of its previous
   run is lost, and each link up connects anew. *)
let light_at sim sink peers =
  sim.light <- Some (sink, peers);
  Peer_map.iter
    (fun peer link ->
      cut sim peer;
      if link.up then
        link_event sim peer (Connected { peer; root = Peer_map.find peer sim.roots }))
    sim.links

(* A full peer is (again) at [sink]; a restart cuts its link, which
   reconnects. *)
let peer_at sim peer sink mount =
  let restarted = Peer_map.mem peer sim.peers in
  sim.peers <- Peer_map.add peer (sink, mount) sim.peers;
  if not (Peer_map.mem peer sim.links) then
    sim.links <- Peer_map.add peer { up = false; slow = false; epoch = 0 } sim.links;
  if restarted && (Peer_map.find peer sim.links).up then (
    cut sim peer;
    link_event sim peer (Disconnected { peer });
    link_event sim peer (Connected { peer; root = Peer_map.find peer sim.roots }))

let slow sim peer = (Peer_map.find peer sim.links).slow <- true

let is_up sim peer =
  match Peer_map.find_opt peer sim.links with Some link -> link.up | None -> false

let connect sim peer =
  match Peer_map.find_opt peer sim.roots with
  | None -> ()
  | Some root ->
      let link = Peer_map.find peer sim.links in
      if not link.up then (
        link.up <- true;
        link_event sim peer (Connected { peer; root }))

let disconnect sim peer =
  let link = Peer_map.find peer sim.links in
  if link.up then (
    link.up <- false;
    cut sim peer;
    link_event sim peer (Disconnected { peer }))

(* Delivers what is due, as the network and the Services' threads would. An
   answer for a node that restarted is refused at its old address, and
   lost. *)
let advance sim now =
  List.iter
    (function
      | Collected (reply, event) -> ignore (Reply.try_send_as_worker reply event)
      | Proved (reply, event) -> ignore (Reply.try_send_as_worker reply event)
      | Verified (reply, event) -> ignore (Reply.try_send_as_worker reply event)
      | Stored { reply; event; write } ->
          (match write with
          | Some (rpc_id, query, root, utxos) -> (
              let key = (rpc_id, query) in
              (match Hydration_map.find_opt key sim.writes with
              | Some 1 -> sim.writes <- Hydration_map.remove key sim.writes
              | Some n -> sim.writes <- Hydration_map.add key (n - 1) sim.writes
              | None -> failwith "a write in flight");
              match event with
              | Partial { ok = true; _ } ->
                  let leaves =
                    Option.value
                      (Root_map.find_opt root sim.partial)
                      ~default:Int_map.empty
                  in
                  let leaves =
                    List.fold_left
                      (fun m (index, utxo) -> Int_map.add index utxo m)
                      leaves utxos
                  in
                  sim.partial <- Root_map.add root leaves sim.partial
              | _ -> ())
          | None -> ());
          ignore (Reply.try_send_as_worker reply event)
      | To_peer { peer; epoch; request_id; request } ->
          if link_up sim peer epoch then
            let sink, mount = Peer_map.find peer sim.peers in
            ignore
              (Sink.try_send_to_as_worker sink mount Full_peer.incoming_message
                 { request_id; request })
      | To_light { peer; epoch; reply; event } ->
          if link_up sim peer epoch then ignore (Reply.try_send_as_worker reply event)
      | Link event -> (
          match sim.light with
          | Some (sink, peers) ->
              ignore (Sink.try_send_to_as_worker sink peers P.link_event_message event)
          | None -> failwith "a light node"))
    (In_flight.due sim.flight now)

let world sim =
  {
    Causet_test.next_time = (fun () -> In_flight.next_time sim.flight);
    advance = advance sim;
  }
