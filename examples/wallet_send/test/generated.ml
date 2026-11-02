(* Generated runs of a light node and its full peers: sends from a few
   owners, hydrated over a simulated network by peers that lag, disconnect,
   restart or lie, with new blocks, mempool transactions, a wallet seeded
   late, and restarts of the light node. The Rust port's
   tests/generated.rs.

   Checked after every root input: the light node's invariants (wallet
   send's, the probe lifecycle's, hydration's, the network layer's), and two
   checks across the nodes: everything the light node holds as checked at a
   root is what the chain had there, a leaf held as present is stored, and
   every probe it verifies is one its peer answered, as asked. Once a run is
   quiet: every send of the current run was answered exactly once, nothing
   stays reserved, no two queued transactions spend the same input, no send
   collected while its hydration's write was in flight, a send rejected for
   a failed write was rejected for its own hydration's, nothing is left in
   flight, and every stored UTXO is the chain's. *)

open Causet
open Wallet_send
open Domain
module Harness = Causet_test.Node
module Rng = Causet_test.Rng
module Swarm = Causet_test.Swarm
module Group = Causet_test.Group
module Scenario = Causet_test.Scenario
module L = Partial_ledger
module P = Peers

let start = 1_000 * Sim.ms
let owners = 3

let faults =
  Full_peer.[| Alter_item; Drop_item; Duplicate_item; Extra_item; Other_root; Silent |]

(* What a run does next. Each run uses some of these, picked by weight; the
   first is the simplest. *)
type step =
  | Deliver  (** A burst of deliveries. *)
  | Send
  | Heartbeat  (** The heartbeat that continues mempool scans. *)
  | Mempool
  | Block
  | Catch_up
  | Link
  | Seed
  | Start  (** An open send started again from outside. *)
  | Cancel  (** An open send cancelled and started again. *)
  | Restart_light
  | Restart_peer

let steps =
  [
    (Deliver, 4);
    (Send, 2);
    (Heartbeat, 1);
    (Mempool, 1);
    (Block, 1);
    (Catch_up, 1);
    (Link, 1);
    (Seed, 1);
    (Start, 1);
    (Cancel, 1);
    (Restart_light, 1);
    (Restart_peer, 1);
  ]

type peer_node = (Full_peer.state, Full_peer.services) Harness.t

type peer = {
  id : P.peer_id;
  node : peer_node;
  mount : Sim.peer_mount;
  mutable at : int;  (** The chain position it caught up to. *)
  mutable session : int;
}

(* What a node shows the group: the light node, the cache entries checked at
   a root and the probes being verified; a peer, the requests it
   answered. *)
type view =
  | Light of {
      checked : (root * int * commitment * bool) list;
      verifying : (P.peer_id * Request_id.t * Ledger.probe_request) list;
    }
  | Peer of Ledger.probe_request Request_map.t

let light_view (state : Node.state) =
  Light
    {
      checked = L.checked state.ledger;
      verifying =
        L.Job_map.bindings state.ledger.jobs
        |> List.filter_map (fun (_, (job : L.job)) ->
               match job.phase with
               | Verify_pending { request_id } -> Some (job.peer, request_id, job.request)
               | _ -> None);
    }

let peer_name (P.Peer_id n) = Printf.sprintf "peer-%d" n

let light_checked views =
  match Group.view views "light" with Some (Light l) -> Some l.checked | _ -> None

(* The checks across nodes. *)
let group_checks group (sim : Sim.t) =
  let ( let* ) = Result.bind in
  let rec all f = function
    | [] -> Ok ()
    | x :: rest ->
        let* () = f x in
        all f rest
  in
  Group.check group "checked leaves are the chain's" (fun views _ ->
      match light_checked views with
      | None -> Ok ()
      | Some checked ->
          all
            (fun (root, index, c, present) ->
              match Sim.ledger_at sim root with
              | None ->
                  Error
                    (Format.asprintf "leaf %d checked at %a, never a root" index pp_root
                       root)
              | Some ledger ->
                  let there =
                    Option.map commitment (Int_map.find_opt index ledger.leaves) = Some c
                  in
                  if there <> present then
                    Error
                      (Format.asprintf "leaf %d at %a: held as %b, the chain says %b"
                         index pp_root root present there)
                  else Ok ())
            checked);
  Group.check group "leaves held as present are stored" (fun views _ ->
      match light_checked views with
      | None -> Ok ()
      | Some checked ->
          all
            (fun (root, index, c, present) ->
              let stored =
                Option.bind (Root_map.find_opt root sim.partial) (Int_map.find_opt index)
              in
              if present && Option.map commitment stored <> Some c then
                Error
                  (Format.asprintf "leaf %d at %a is held as present, not stored" index
                     pp_root root)
              else Ok ())
            checked);
  Group.check group "probes verified are the ones answered" (fun views _ ->
      match Group.view views "light" with
      | Some (Light { verifying; _ }) ->
          all
            (fun (peer, request_id, request) ->
              let answered =
                match Group.view views (peer_name peer) with
                | Some (Peer answered) -> Request_map.find_opt request_id answered
                | _ -> None
              in
              match answered with
              | Some asked when asked = request -> Ok ()
              | other ->
                  Error
                    (Format.asprintf "%a is verified for %a, answered as %a" P.pp_peer_id
                       peer Request_id.pp request_id
                       (Debug.option Ledger.pp_probe_request)
                       other))
            verifying
      | _ -> Ok ())

let new_utxo sim rng =
  let owner = Owner (1 + Rng.below rng owners) in
  let nonce = Sim.fresh_nonce sim in
  base owner (1 + Rng.below rng 40) nonce

(* The wallet learns of [ledger]'s UTXOs, each with chance [numerator] in
   4. *)
let learn light mount ledger rng numerator =
  for owner = 1 to owners do
    let entries =
      Ledger.owned ledger (Owner owner)
      |> List.filter (fun _ -> Rng.chance rng numerator 4)
      |> List.map (fun (index, (utxo : utxo)) -> (index, commitment utxo, utxo.amount))
    in
    if entries <> [] then
      ignore
        (Harness.dispatch light mount (L.Wallet_seen { owner = Owner owner; entries }))
  done

let fail fmt = Printf.ksprintf failwith fmt

let generated lossy rng =
  let scenario = Scenario.with_rng rng start in
  (* The world draws from a stream of its own. *)
  let sim = Sim.create (Rng.split rng) (Scenario.clock scenario) lossy Ledger.empty in
  let genesis =
    let n = Rng.between rng 2 (4 * owners) in
    let rec add ledger i =
      if i = 0 then ledger
      else
        let utxo = new_utxo sim rng in
        let index = Option.get (Ledger.free_leaf ledger) in
        add { Ledger.leaves = Int_map.add index utxo ledger.Ledger.leaves } (i - 1)
    in
    add Ledger.empty n
  in
  Dynarray.clear sim.chain;
  Dynarray.add_last sim.chain (Ledger.root genesis, genesis);
  let group = Group.create "ledger" in
  let session = ref 1 in
  let services, timers = Sim.light_services sim in
  let light =
    Harness.create ~name:"light" Node.component
      ~state:(Node.create (Block_hash 0L) (Ledger.root genesis))
      ~services ~session:!session (Scenario.clock scenario)
  in
  let light =
    Harness.publish
      (Harness.check (Harness.with_timer light timers) "light node" Node.invariants)
      group light_view
  in
  let at = Path.root Node.component in
  let mount = Harness.mount light at in
  let ledger_mount = Harness.mount light (Path.child at Node.ledger_slot) in
  let peers_mount = Harness.mount light (Path.child at Node.peers_slot) in
  Sim.light_at sim (Harness.sink light) peers_mount;
  let peer_root = Path.root Full_peer.component in
  let peers =
    Array.init (Rng.between rng 1 3) (fun i ->
        let id = P.Peer_id (i + 1) in
        let fault =
          if Rng.chance rng 1 2 then Full_peer.Honest else Rng.pick rng faults
        in
        let node =
          Harness.create ~name:(peer_name id) Full_peer.component
            ~state:(Full_peer.create id fault) ~services:(Sim.peer_services sim)
            ~session:1 (Scenario.clock scenario)
        in
        let node =
          Harness.publish node group (fun (s : Full_peer.state) -> Peer s.answered)
        in
        let peer_mount = Harness.mount node peer_root in
        Sim.peer_at sim id (Harness.sink node) peer_mount;
        if Rng.chance rng 1 4 then Sim.slow sim id;
        ignore (Harness.dispatch node peer_mount (Caught_up { ledger = genesis }));
        Sim.connect sim id;
        { id; node; mount = peer_mount; at = 0; session = 1 })
  in
  group_checks group sim;
  learn light ledger_mount genesis rng 3;
  let seeded = ref (Rng.chance rng 2 3) in
  if !seeded then ignore (Harness.dispatch light ledger_mount L.Seeded);
  let tip = ref 0 and next_rpc = ref 1 and this_run = ref Rpc_set.empty in
  let drivens =
    Causet_test.driven light
    :: List.map (fun p -> Causet_test.driven p.node) (Array.to_list peers)
  in
  let world = Sim.world sim in
  let step () = Scenario.step scenario drivens world in
  let dispatch action = ignore (Harness.dispatch light mount action) in
  let open_rpcs () = List.map fst (Rpc_map.bindings (Harness.state light).rpc) in
  let swarm = Swarm.create rng steps in
  while Rng.more rng do
    match Swarm.pick swarm rng with
    (* A burst of deliveries, so sends finish between other steps and an
       owner can send again. *)
    | Deliver ->
        for _ = 1 to Rng.between rng 1 8 do
          ignore (step ())
        done
    | Send ->
        (* Owner 1 sends most, so sends of one owner follow each other. *)
        let from = Owner (if Rng.chance rng 1 2 then 1 else 1 + Rng.below rng owners) in
        let to_ = Owner (1 + Rng.below rng owners) in
        let amount = 1 + Rng.below rng 60 in
        let fee = Some (Rng.below rng 3) in
        let req : send_request = { from; to_; amount; fee; memo = "" } in
        let rpc_id = Rpc_id !next_rpc in
        incr next_rpc;
        this_run := Rpc_set.add rpc_id !this_run;
        dispatch (Send { rpc_id; req })
    | Heartbeat -> dispatch Check_timeouts
    | Mempool ->
        let outputs = [ new_utxo sim rng ] in
        dispatch (Mempool_tx { inputs = []; outputs })
    | Block ->
        (* A new block: a UTXO spent elsewhere, one or two new ones, which
           the wallet usually learns of. *)
        let ledger = snd (Sim.newest sim) in
        let ledger =
          if Rng.chance rng 1 3 && not (Int_map.is_empty ledger.leaves) then
            let leaves = Array.of_list (List.map fst (Int_map.bindings ledger.leaves)) in
            { Ledger.leaves = Int_map.remove (Rng.pick rng leaves) ledger.leaves }
          else ledger
        in
        let ledger = ref ledger and added = ref Ledger.empty in
        for _ = 1 to Rng.between rng 1 2 do
          let utxo = new_utxo sim rng in
          match Ledger.free_leaf !ledger with
          | Some index ->
              ledger := { Ledger.leaves = Int_map.add index utxo !ledger.leaves };
              added := { Ledger.leaves = Int_map.add index utxo !added.leaves }
          | None -> ()
        done;
        let ledger = !ledger in
        let root = Ledger.root ledger in
        Dynarray.add_last sim.chain (root, ledger);
        incr tip;
        dispatch (New_tip { tip = Block_hash (Int64.of_int !tip); root });
        learn light ledger_mount !added rng 3;
        (* Most peers follow the chain closely. *)
        let newest = Dynarray.length sim.chain - 1 in
        Array.iter
          (fun peer ->
            if Rng.chance rng 2 3 then (
              peer.at <- newest;
              ignore (Harness.dispatch peer.node peer.mount (Caught_up { ledger }))))
          peers
    | Catch_up ->
        (* A peer catches up: one block, or to the tip. *)
        let newest = Dynarray.length sim.chain - 1 in
        let peer = peers.(Rng.below rng (Array.length peers)) in
        if peer.at < newest then (
          peer.at <- (if Rng.chance rng 1 2 then newest else peer.at + 1);
          let ledger = snd (Dynarray.get sim.chain peer.at) in
          ignore (Harness.dispatch peer.node peer.mount (Caught_up { ledger })))
    | Link ->
        let id = peers.(Rng.below rng (Array.length peers)).id in
        if Sim.is_up sim id then Sim.disconnect sim id else Sim.connect sim id
    | Seed ->
        if not !seeded then (
          let ledger = snd (Sim.newest sim) in
          learn light ledger_mount ledger rng 4;
          ignore (Harness.dispatch light ledger_mount L.Seeded);
          seeded := true)
    (* An open send started again from outside, possibly while its hydration
       probes or stores, which the node refuses. *)
    | Start -> (
        match open_rpcs () with
        | [] -> ()
        | open_ ->
            let rpc_id = List.nth open_ (Rng.below rng (List.length open_)) in
            dispatch (Start { rpc_id }))
    (* An open send cancelled and started again: a replacement attempt,
       whose hydration may meet what the cancelled one left in flight. No
       send waits for those writes any more. Usually the run first goes on
       until some send's hydration is storing, a short window a random step
       would rarely hit. *)
    | Cancel -> (
        let storing () =
          let state = Harness.state light in
          List.find_opt
            (fun rpc_id ->
              match Rpc_map.find_opt rpc_id state.ledger.hydrations with
              | Some { stage = Storing _; _ } -> true
              | _ -> false)
            (open_rpcs ())
        in
        let target = ref None in
        (if Rng.chance rng 2 3 then
           let rec go n =
             if n > 0 then (
               target := storing ();
               if !target = None && step () <> None then go (n - 1))
           in
           go 200);
        let target =
          match !target with
          | Some _ as t -> t
          | None -> (
              match open_rpcs () with
              | [] -> None
              | open_ -> Some (List.nth open_ (Rng.below rng (List.length open_))))
        in
        match target with
        | None -> ()
        | Some rpc_id ->
            (match L.hydration_query (Harness.state light).ledger rpc_id with
            | Some query -> sim.abandoned <- Sim.Request_set.add query sim.abandoned
            | None -> ());
            dispatch (Cancel { rpc_id });
            dispatch (Start { rpc_id }))
    | Restart_light ->
        (* The light node restarts: it keeps the chain, the mempool and the
           wallet cache; the RPCs, the work in progress, the connections and
           the probes are gone. *)
        incr session;
        let kept = Harness.state light in
        let kept : Node.state =
          {
            kept with
            rpc = Rpc_map.empty;
            wallet_send = State.empty;
            deadlines = Pending.empty;
            peers = P.empty;
            ledger =
              {
                kept.ledger with
                jobs = L.Job_map.empty;
                hydrations = Rpc_map.empty;
                attempted = Rpc_set.empty;
              };
          }
        in
        let services, timers = Sim.light_services sim in
        Harness.restart light ~state:kept ~services ~session:!session;
        ignore (Harness.with_timer light timers);
        Sim.light_at sim (Harness.sink light) peers_mount;
        this_run := Rpc_set.empty
    | Restart_peer ->
        (* A full peer restarts, keeping its ledgers. *)
        let peer = peers.(Rng.below rng (Array.length peers)) in
        peer.session <- peer.session + 1;
        let kept = Harness.state peer.node in
        Harness.restart peer.node ~state:kept ~services:(Sim.peer_services sim)
          ~session:peer.session;
        Sim.peer_at sim peer.id (Harness.sink peer.node) peer.mount
  done;
  (* The world calms down: every peer caught up and connected, the wallet
     seeded. Then the run goes quiet, the heartbeat given whenever a mempool
     scan waits for it. *)
  let newest = Dynarray.length sim.chain - 1 in
  Array.iter
    (fun peer ->
      if peer.at < newest then (
        peer.at <- newest;
        let ledger = snd (Dynarray.get sim.chain newest) in
        ignore (Harness.dispatch peer.node peer.mount (Caught_up { ledger })));
      Sim.connect sim peer.id)
    peers;
  if not !seeded then ignore (Harness.dispatch light ledger_mount L.Seeded);
  let rec settle n =
    if n = 0 then fail "mempool scans never finished"
    else (
      (match
         Scenario.run_until scenario drivens world ~steps:100_000 ~until:(fun () -> false)
       with
      | Causet_test.Quiet -> ()
      | _ -> fail "not quiet: %s" (Scenario.pending scenario drivens world));
      if State.next_mempool_scan_request (Harness.state light).wallet_send <> Ok None then (
        dispatch Check_timeouts;
        settle (n - 1)))
  in
  settle 1_000;
  if sim.violations <> [] then fail "[%s]" (String.concat "; " (List.rev sim.violations));
  Rpc_map.iter
    (fun rpc_id responses ->
      if List.length responses <> 1 then
        fail "%s was answered %d times: %s"
          (Debug.to_string pp_rpc_id rpc_id)
          (List.length responses)
          (Debug.to_string (Debug.list pp_send_response) responses))
    sim.responses;
  Rpc_set.iter
    (fun rpc_id ->
      if not (Rpc_map.mem rpc_id sim.responses) then
        fail "%s was never answered" (Debug.to_string pp_rpc_id rpc_id))
    !this_run;
  let state = Harness.state light in
  if not (Rpc_map.is_empty state.rpc) then
    fail "RPCs left pending: %s"
      (Debug.to_string
         (pp_map pp_rpc_id Node.pp_rpc_request)
         (Rpc_map.bindings state.rpc));
  if State.working state.wallet_send <> [] then fail "work left in progress";
  for owner = 1 to owners do
    if not (State.owner_is_available state.wallet_send (Rpc_id max_int) (Owner owner))
    then fail "%d left reserved" owner
  done;
  if not (L.Job_map.is_empty state.ledger.jobs) then
    fail "probes left: %s"
      (Debug.to_string (Debug.list L.pp_job)
         (List.map snd (L.Job_map.bindings state.ledger.jobs)));
  if not (Rpc_map.is_empty state.ledger.hydrations) then
    fail "hydrations left: %s"
      (Debug.to_string (Debug.list L.pp_hydration)
         (List.map snd (Rpc_map.bindings state.ledger.hydrations)));
  if not (Request_map.is_empty state.peers.outstanding) then fail "requests left out";
  (* No two queued transactions spend the same input. *)
  ignore
    (Rpc_map.fold
       (fun _ responses spent ->
         List.fold_left
           (fun spent (r : send_response) ->
             match r.tx_id with
             | None -> spent
             | Some tx_id ->
                 List.fold_left
                   (fun spent input ->
                     if Commitment_set.mem input spent then
                       fail "%s is spent twice" (Debug.to_string pp_commitment input);
                     Commitment_set.add input spent)
                   spent (Int_map.find tx_id sim.proved).inputs)
           spent responses)
       sim.responses Commitment_set.empty);
  (* Every UTXO stored as proved is the chain's at its root. *)
  Root_map.iter
    (fun root leaves ->
      match Sim.ledger_at sim root with
      | None -> fail "%s is not a root of the chain" (Debug.to_string pp_root root)
      | Some ledger ->
          Int_map.iter
            (fun index utxo ->
              if Int_map.find_opt index ledger.leaves <> Some utxo then
                fail "stored leaf %d at %s" index (Debug.to_string pp_root root))
            leaves)
    sim.partial

let runs = 5_000

let generated_light_nodes_keep_their_invariants_and_answer_once () =
  Causet_test.check ~config:{ Causet_test.default with runs } (generated false)

(* Services that lose answers, or send them after the node's deadline: a
   send then times out instead of waiting forever, is answered once, and a
   late answer changes nothing. (Before the deadlines, generation found the
   send waiting forever and shrank it to one send whose one answer was
   lost.) *)
let lost_and_late_answers_end_in_a_timeout () =
  Causet_test.check ~config:{ Causet_test.default with runs } (generated true)

let () =
  let t name f = Alcotest.test_case name `Slow f in
  Alcotest.run "wallet send generated"
    [
      ( "generated",
        [
          t "generated light nodes keep their invariants and answer once"
            generated_light_nodes_keep_their_invariants_and_answer_once;
          t "lost and late answers end in a timeout"
            lost_and_late_answers_end_in_a_timeout;
        ] );
    ]
