(* Generated runs of two to four nodes over a simulated network: fetches
   between them, links that connect, lose messages, fail sends at once and
   break (with the dials in flight between the pair, at times), nodes that
   dial each other at once, disconnect and reconnect (each new connection
   numbering its wire IDs from 0 again), in half the runs parents that
   connect again whenever told a peer they want is gone, stale callbacks
   delivered on purpose, policy changes, and a server that answers at once,
   late, or never. The Rust port's examples/p2p-node/tests/generated.rs.

   The network is as a real transport is: each end numbers its own
   connections, a dial when it starts and an inbound connection when it is
   accepted, so the two ends of a link know it by different numbers and the
   order of numbers is not the order of establishment; a dial takes a while
   to connect; a pair can have several links up, some opened by the network
   itself; and a peer may send several requests at once.

   Checked after every root input: each node's invariants (P2P's limits;
   routes and reply tokens agreeing with what P2P holds), and across the
   nodes that every answer a node received is the one its peer sent for that
   very request. From the node's executions: no request's abort is reported
   after its peer's disconnection; P2P closes a connection only when the
   node disconnects the peer, after a failed send on it, or when it is a
   connection arriving that it will not use, never the answer to the dial it
   waits for, nor below the limit one the peer opened; only that dial fails,
   and only it or a connection the peer opened makes the peer ready; ready
   and gone are each reported once in turn (gone again only after a
   connect); a connect to a peer P2P holds no connection to dials, unless a
   dial is out, and no dial goes out while another is; a request is reported
   failed only after the failed send of that request, and answered only by a
   response on its own connection with its wire ID; and in the network, a
   node answers only on the link the request came on. Once quiet: every
   fetch offered has exactly one outcome, nothing is left in flight, both
   ends hold every link that is up and nothing else, each node was last told
   of each peer what it is, and told something after each connect that found
   it holding none (the dial's outcome, or a connection the peer opened), a
   parent that retries is connected to every peer it wants, and over the
   whole check at least half the fetches were accepted, a fifth answered,
   half the bounces with work in flight reached a new connection, and the
   retrying runs retried, so that refusing everything cannot pass. *)

open Causet
open P2p
open P2p_node
open Sim_common
open Sim_network
open Sim_checks

(* ------------------------------------------------------------------------ *)
(* Runs *)

type step =
  | Deliver
  | Fetch_step
  | Connect_step
  | Simultaneous  (** Two nodes dial each other at once. *)
  | Extra  (** The network opens another connection between two nodes. *)
  | Flood  (** A peer sends six requests at once, and answers nobody asked for. *)
  | Disconnect_step
  | Break
      (** One link of a pair breaks; half the time, the dials between them in flight fail
          too. *)
  | Stale
  | Policy
  | Overtaken
      (** A node connects, and a connection the peer opened overtakes its dial; then the
          pair is cut off, so the node is told the peer gone with its dial still out, and
          connects again, which keeps the dial. However it ends (it fails, or its
          connection comes and is cut, or it failed before the cut and was not kept), the
          node must hear it. *)
  | Bounce
      (** Every link of a pair whose server is working on an answer breaks; the requester,
          once it holds no connection, dials, and asks again as soon as it holds a new
          one, which numbers wire IDs from 0 again, as the old request's. *)

let steps =
  [
    (Deliver, 4);
    (Fetch_step, 4);
    (Connect_step, 2);
    (Simultaneous, 1);
    (Extra, 1);
    (Flood, 1);
    (Disconnect_step, 1);
    (Break, 1);
    (Stale, 1);
    (Policy, 1);
    (Bounce, 1);
    (Overtaken, 1);
  ]

type test_node = (node_state, node_services) Harness.t

(* The nodes' parents: what each wants to be connected to, and, in a run
   that retries, connecting again to a peer it wants as soon as it is told
   the peer is gone. *)
type parents = {
  retry : bool;
  mutable wants : Want_set.t;
  mounts : node_mount array;
  parent_causes : causes array;
}

let connect parents (all : test_node array) me peer =
  parents.wants <- Want_set.add (me, peer) parents.wants;
  ignore (Harness.dispatch all.(me) parents.mounts.(me) (Connect { peer }))

let disconnect parents (all : test_node array) me peer =
  parents.wants <- Want_set.remove (me, peer) parents.wants;
  ignore (Harness.dispatch all.(me) parents.mounts.(me) (Disconnect { peer }))

(* The retries owed, then one step of the scenario. False once there is
   nothing left to do. *)
let parents_step parents scenario (all : test_node array) drivens world =
  let retried = ref false in
  Array.iteri
    (fun me node ->
      let gone = parents.parent_causes.(me).gone in
      parents.parent_causes.(me).gone <- [];
      List.iter
        (fun peer ->
          if parents.retry && Want_set.mem (me, peer) parents.wants then (
            ignore (Harness.dispatch node parents.mounts.(me) (Connect { peer }));
            incr retries;
            retried := true))
        gone)
    all;
  Scenario.step scenario drivens world <> None || !retried

let generated rng =
  let scenario = Scenario.with_rng rng start in
  let clock = Scenario.clock scenario in
  let net =
    {
      world = Rng.split rng;
      clock;
      nodes = Peer_map.empty;
      links = Int_map.empty;
      next_link = 0;
      numbers = Peer_map.empty;
      by_end = End_map.empty;
      flight = In_flight.create ();
      delivered = Dynarray.create ();
      requests_in = Request_set.empty;
      cut = Int_set.empty;
      violations = [];
    }
  in
  let world = world net in
  let group = Group.create "p2p" in
  let ids = List.init (Rng.between rng 2 4) (fun n -> Peer_id n) in
  let made =
    List.map
      (fun id ->
        let timer, control = Timer.simulated clock in
        let services =
          { p2p = { transport = transport net id; timer }; server = server net id }
        in
        let node =
          Harness.create ~name:(name id) P2p_node.node ~state:(new_state id) ~services
            ~session:1 clock
        in
        let node =
          Harness.publish
            (Harness.check (Harness.with_timer node control) "node" P2p_node.invariants)
            group view
        in
        let order = { gone_last = Peer_map.empty; order_violations = [] } in
        Store.add_observer (Harness.store node) (order_observer order);
        let cause = new_causes () in
        Store.add_observer (Harness.store node) (causes_observer cause);
        let account =
          {
            pending_callbacks = Int_set.empty;
            pending_intents = Int_set.empty;
            account_violations = [];
          }
        in
        Store.add_observer (Harness.store node) (accounts_observer account);
        let root = Path.root P2p_node.node in
        let p2p = Harness.mount node (Path.child root p2p_slot) in
        net.nodes <- Peer_map.add id (Harness.sink node, p2p) net.nodes;
        (node, Harness.mount node root, order, cause, account))
      ids
  in
  let all = Array.of_list (List.map (fun (n, _, _, _, _) -> n) made) in
  let mounts = Array.of_list (List.map (fun (_, m, _, _, _) -> m) made) in
  let orders = List.map (fun (_, _, o, _, _) -> o) made in
  let causes = Array.of_list (List.map (fun (_, _, _, c, _) -> c) made) in
  let accounts = List.map (fun (_, _, _, _, a) -> a) made in
  let drivens = Array.to_list (Array.map Causet_test.driven all) in
  let ids_a = Array.of_list ids in
  let count = Array.length ids_a in
  group_checks group ids;
  let retry = Rng.chance rng 1 2 in
  let parents = { retry; wants = Want_set.empty; mounts; parent_causes = causes } in
  let step () = parents_step parents scenario all drivens world in
  let other_index rng me =
    let peer = Rng.below rng (count - 1) in
    if peer >= me then peer + 1 else peer
  in
  let other rng me = ids_a.(other_index rng me) in
  let state i = Harness.state all.(i) in
  (* Steps until [stop] holds, at most 50 times. *)
  let step_until stop =
    let rec go n =
      if n > 0 && not (stop ()) then (
        ignore (step ());
        go (n - 1))
    in
    go 50
  in

  (* Every pair connects first. *)
  for i = 0 to count - 1 do
    for b = i + 1 to count - 1 do
      connect parents all i ids_a.(b)
    done
  done;
  (let rec go n = if n > 0 && step () then go (n - 1) in
   go 10_000);

  let swarm = Swarm.create rng steps in
  while Rng.more rng do
    let kind = Swarm.pick swarm rng in
    let me = Rng.below rng count in
    match kind with
    | Deliver ->
        for _ = 1 to Rng.between rng 1 8 do
          ignore (step ())
        done
    | Fetch_step ->
        (* A peer ready to take a request, mostly. *)
        let p2p = p2p_of all.(me) in
        let free =
          List.filter
            (fun peer -> N.is_ready p2p peer && N.outbound_to p2p peer = None)
            ids
        in
        (* With none free, a fetch is mostly not offered; one in four is, and
           refused. *)
        if not (free = [] && not (Rng.chance rng 1 4)) then
          let peer =
            if free = [] || Rng.chance rng 1 5 then other rng me
            else List.nth free (Rng.below rng (List.length free))
          in
          let key = Rng.below rng 20 in
          ignore (Harness.dispatch all.(me) mounts.(me) (Fetch { peer; key }))
    | Connect_step -> connect parents all me (other rng me)
    | Simultaneous ->
        let them = other_index rng me in
        let a = ids_a.(me) and b = ids_a.(them) in
        connect parents all me b;
        connect parents all them a
    | Extra -> open_extra net ids_a.(me) (other rng me)
    | Flood -> flood net ids_a.(me) (other rng me)
    | Disconnect_step -> disconnect parents all me (other rng me)
    | Break ->
        let peer = other rng me in
        let up = links_between net ids_a.(me) peer in
        if up <> [] then break_link net (List.nth up (Rng.below rng (List.length up)));
        (* Half the time the pair is cut off for a moment: the dials between
           them in flight fail too. *)
        if Rng.chance rng 1 2 then cut_dials net ids_a.(me) peer
    | Stale ->
        let pick = Rng.next rng in
        let responses = Rng.chance rng 2 3 in
        replay_stale net pick responses
    | Bounce ->
        let working =
          List.find_map
            (fun server ->
              match Token_set.min_elt_opt (state server).tokens with
              | None -> None
              | Some token ->
                  Option.map
                    (fun requester -> (requester, server))
                    (List.find_index (fun id -> id = token.peer) ids))
            (List.init count Fun.id)
        in
        (* Drawn whether or not it is used, as Rust's [unwrap_or] does. *)
        let fallback = (me, other_index rng me) in
        let requester, server = Option.value working ~default:fallback in
        let peer = ids_a.(server) in
        let held () = N.connections (p2p_of all.(requester)) peer in
        let old = held () in
        (* Counted only with work in flight, on connections to break. *)
        let counted = working <> None && not (Connection_set.is_empty old) in
        if counted then incr bounces;
        List.iter (break_link net) (links_between net ids_a.(requester) peer);
        (* The requester hears its connections closed, dials, and holds
           another. *)
        step_until (fun () -> Connection_set.is_empty (held ()));
        connect parents all requester peer;
        let fresh () =
          Connection_set.exists (fun c -> not (Connection_set.mem c old)) (held ())
        in
        step_until fresh;
        if fresh () then (
          if counted then incr rebound;
          let key = 10 + Rng.below rng 10 in
          ignore
            (Harness.dispatch all.(requester) mounts.(requester) (Fetch { peer; key })))
    | Overtaken ->
        let them = other_index rng me in
        let peer = ids_a.(them) in
        if Connection_set.is_empty (N.connections (p2p_of all.(me)) peer) then (
          connect parents all me peer;
          open_extra net peer ids_a.(me);
          let holds () =
            not (Connection_set.is_empty (N.connections (p2p_of all.(me)) peer))
          in
          step_until holds;
          List.iter (break_link net) (links_between net ids_a.(me) peer);
          cut_dials net ids_a.(me) peer;
          step_until (fun () -> not (holds ()));
          (* Every attempt counted; [kept], those that reached the point:
             told gone, the dial out. *)
          incr overtaken;
          if (not (holds ())) && N.is_dialing (p2p_of all.(me)) peer then incr kept;
          connect parents all me peer)
    | Policy ->
        let fetch = not (Rng.chance rng 1 5) in
        let serve = not (Rng.chance rng 1 5) in
        ignore
          (Harness.dispatch all.(me) mounts.(me)
             (Set_policy { policy = { fetch; serve } }))
  done;
  let quiet =
    let rec go n = n > 0 && ((not (step ())) || go (n - 1)) in
    go 100_000
  in
  if not quiet then failwith "still busy after 100,000 steps: no end";

  let fail_on id = function
    | [] -> ()
    | violations ->
        failwith
          (Format.asprintf "%a: [%s]" pp_peer_id id
             (String.concat "; " (List.rev violations)))
  in
  List.iter2 (fun id o -> fail_on id o.order_violations) ids orders;
  List.iteri (fun i id -> fail_on id causes.(i).cause_violations) ids;
  List.iter2 (fun id a -> fail_on id a.account_violations) ids accounts;
  if net.violations <> [] then
    failwith (Printf.sprintf "[%s]" (String.concat "; " (List.rev net.violations)));
  links_agree net all ids causes;
  (* A parent that connects again whenever told a peer it wants is gone ends
     connected to it. *)
  if parents.retry then (
    incr retrying;
    Want_set.iter
      (fun (me, peer) ->
        if not (N.is_ready (p2p_of all.(me)) peer) then
          failwith
            (Format.asprintf "%a wants %a, retried on every gone, and is not connected"
               pp_peer_id ids_a.(me) pp_peer_id peer))
      parents.wants);
  Array.iter
    (fun node ->
      let state = Harness.state node in
      Correlation_map.iter
        (fun correlation (fetch : fetch) ->
          if fetch.outcome = None then
            failwith (Format.asprintf "%a has no outcome" pp_correlation correlation);
          incr offered;
          (* A flood's unasked answer carries no value: not counted. *)
          match fetch.outcome with
          | Some (Answered (Some _)) -> incr answered
          | Some Refused -> incr refused
          | _ -> ())
        state.fetches;
      if not (Correlation_map.is_empty state.routes) then failwith "routes left";
      if not (Token_set.is_empty state.tokens) then failwith "reply tokens left";
      if N.inbound_count state.p2p <> 0 then failwith "inbound requests left";
      if N.outbound state.p2p <> [] then failwith "outbound requests left")
    all

let generated_nodes_keep_their_requests_straight () =
  Causet_test.check ~config:{ Causet_test.default with runs = 10_000 } generated;
  (* Refusing everything would end every fetch once: generation must have
     most fetches accepted, and many answered. *)
  if !refused * 2 > !offered then
    Alcotest.failf "%d of %d fetches refused" !refused !offered;
  if !answered * 5 < !offered then
    Alcotest.failf "only %d of %d answered" !answered !offered;
  (* Runs whose parents retry must retry: about three times a run. *)
  if !retries < !retrying then
    Alcotest.failf "only %d retries over %d retrying runs" !retries !retrying;
  (* An overtaken dial must often reach its point, a gone with the dial still
     out: over half of them do, and the floor is a third. *)
  if !kept * 3 < !overtaken then
    Alcotest.failf "only %d of %d overtaken dials were still out when told gone" !kept
      !overtaken;
  (* Refusals are told: stale callbacks and Intents P2P refuses must come
     up, or the accounts check nothing. About one callback in 26 and one
     Intent in 3.5 are refused. *)
  if !callbacks_refused * 50 < !callbacks then
    Alcotest.failf "only %d of %d callbacks refused" !callbacks_refused !callbacks;
  if !intents_refused * 8 < !intents then
    Alcotest.failf "only %d of %d Intents refused" !intents_refused !intents;
  if !rebound * 2 < !bounces then
    Alcotest.failf "only %d of %d bounces with work in flight reached a new connection"
      !rebound !bounces;
  Printf.printf
    "offered %d, refused %d, answered %d; retries %d over %d retrying runs; overtaken \
     %d, kept %d; bounces %d, rebound %d; callbacks %d, refused %d; Intents %d, refused \
     %d\n\
     %!"
    !offered !refused !answered !retries !retrying !overtaken !kept !bounces !rebound
    !callbacks !callbacks_refused !intents !intents_refused

let () =
  Alcotest.run "p2p generated"
    [
      ( "generated",
        [
          Alcotest.test_case "generated nodes keep their requests straight" `Slow
            generated_nodes_keep_their_requests_straight;
        ] );
    ]
