(* Each of the P2P component's contracts, step by step, on one node whose
   transport and server record what they are asked, and whose transport
   callbacks the tests send by hand. The Rust port's
   examples/p2p-node/tests/contracts.rs, test for test. *)

open Causet
open P2p
open P2p_node
module Harness = Causet_test.Node
module N = Node_p2p

type asked =
  | Dial of peer_id * Dial_id.t
  | Send of peer_id * connection_id * N.wire
  | Close of peer_id * connection_id

type record = {
  mutable asked : asked list;  (** in the order asked *)
  mutable serves : (reply_token * req * served reply) list;
  mutable outputs : string list;  (** P2P's Outputs, in the order the node lowered them. *)
}

let transport record : N.transport =
  {
    dial = (fun peer dial -> record.asked <- record.asked @ [ Dial (peer, dial) ]);
    send =
      (fun peer connection message ->
        record.asked <- record.asked @ [ Send (peer, connection, message) ]);
    close =
      (fun peer connection -> record.asked <- record.asked @ [ Close (peer, connection) ]);
  }

let server record =
  {
    serve =
      (fun token request reply ->
        record.serves <- record.serves @ [ (token, request, reply) ]);
  }

(* Records the kind of every P2P Output the node lowers, with its peer. *)
let outputs_observer record =
  let line (output : N.output) =
    let name, Peer_id peer =
      match output with
      | Peer_ready { peer } -> ("ready", peer)
      | Peer_disconnected { peer } -> ("gone", peer)
      | Rpc_request_started { peer; _ } -> ("started", peer)
      | Rpc_response_received { peer; _ } -> ("answered", peer)
      | Rpc_timed_out { peer; _ } -> ("timed out", peer)
      | Rpc_request_aborted { peer; _ } -> ("aborted", peer)
      | Rpc_failed { peer; _ } -> ("failed", peer)
      | Rpc_request_received { peer; _ } -> ("received", peer)
      | Rpc_request_expired { peer; _ } -> ("expired", peer)
      | Rpc_request_dropped { peer; _ } -> ("dropped", peer)
    in
    Printf.sprintf "%s %d" name peer
  in
  {
    Observe.none with
    action_start =
      (fun _ payload ->
        match Message.get node_action_message payload with
        | Some (From_p2p output) -> record.outputs <- record.outputs @ [ line output ]
        | _ -> ());
  }

let peer = Peer_id 7
let me = Peer_id 1

type fixture = {
  node : (node_state, node_services) Harness.t;
  mount : (node_state, node_services, node_action, node_effect, never) mount;
  p2p : (N.state, N.services, N.action, N.effect_, N.output) mount;
  record : record;
  clock : Clock.Manual.m;
}

let fixture () =
  let record = { asked = []; serves = []; outputs = [] } in
  let clock = Clock.Manual.create 1_000_000_000 in
  let timer, control = Timer.simulated clock in
  let services =
    { p2p = { transport = transport record; timer }; server = server record }
  in
  let node =
    Harness.create ~name:"node" P2p_node.node ~state:(new_state me) ~services ~session:1
      clock
  in
  let node = Harness.check (Harness.with_timer node control) "node" P2p_node.invariants in
  Store.add_observer (Harness.store node) (outputs_observer record);
  let root = Path.root P2p_node.node in
  {
    node;
    mount = Harness.mount node root;
    p2p = Harness.mount node (Path.child root p2p_slot);
    record;
    clock;
  }

let state f = Harness.state f.node
let act f action = ignore (Harness.dispatch f.node f.mount action)

let deliver_all f =
  while Harness.deliver_next f.node <> None do
    ()
  done

let callback f event =
  (match
     Sink.try_send_to_as_worker (Harness.sink f.node) f.p2p N.transport_event_message
       event
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the callback was not sent");
  deliver_all f

(* The dials asked of the transport, in order. *)
let dials f =
  List.filter_map (function Dial (_, dial) -> Some dial | _ -> None) f.record.asked

let last list = match List.rev list with [] -> Alcotest.fail "none" | x :: _ -> x

let connected f connection dial =
  callback f (N.Connected { peer; connection = Connection_id connection; dial })

(* The last dial, answered on [connection]. *)
let dialed f connection =
  let dial = last (dials f) in
  connected f connection (Some dial)

(* A connection the peer opened. *)
let opened f connection = connected f connection None

let closes f =
  List.filter_map
    (function Close (_, connection) -> Some connection | _ -> None)
    f.record.asked

let connections f = Connection_set.elements (N.connections (state f).p2p peer)

let closed f connection =
  callback f (N.Closed { peer; connection = Connection_id connection })

let after f duration =
  Clock.Manual.advance f.clock duration;
  ignore (Harness.fire_timers f.node);
  deliver_all f

let outcome f tag = (Correlation_map.find (Correlation tag) (state f).fetches).outcome

(* The last request sent to the peer, with its wire ID. *)
let last_request f =
  match
    List.find_map
      (function
        | Send (_, connection, N.Request { wire; request }) ->
            Some (connection, wire, request)
        | _ -> None)
      (List.rev f.record.asked)
  with
  | Some sent -> sent
  | None -> Alcotest.fail "a request sent"

let responses_sent f =
  List.filter_map
    (function
      | Send (_, connection, N.Response { wire; response }) ->
          Some (connection, wire, response)
      | _ -> None)
    f.record.asked

let outputs f = f.record.outputs
let request key tag : req = { key; tag }
let answer key tag = resp_of peer (request key tag)

(* Takes the first serve the server was asked. *)
let take_serve f =
  match f.record.serves with
  | [] -> Alcotest.fail "a serve"
  | first :: rest ->
      f.record.serves <- rest;
      first

let reply_served f (token, request, reply) =
  let response = Some (resp_of me request) in
  (match Reply.try_send_as_worker reply { token; response } with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the answer was not sent");
  deliver_all f

(* ------------------------------------------------------------------------ *)
(* Checks *)

let connection_ids = Alcotest.testable (Debug.list pp_connection_id) ( = )
let outcome_t = Alcotest.testable (Debug.option pp_outcome) ( = )
let strings = Alcotest.(list string)

let response_sent =
  Alcotest.testable
    (fun f (connection, wire, response) ->
      Format.fprintf f "(%a, %a, %a)" pp_connection_id connection pp_wire_id wire
        (Debug.option pp_resp) response)
    ( = )

let check_connections name expected actual =
  Alcotest.check connection_ids name (List.map (fun c -> Connection_id c) expected) actual

let check_outcome name expected actual = Alcotest.check outcome_t name expected actual
let check_outputs name expected f = Alcotest.check strings name expected (outputs f)
let check_true name b = Alcotest.(check bool) name true b
let tokens f = Token_set.cardinal (state f).tokens

(* ------------------------------------------------------------------------ *)
(* The contracts *)

let a_fetch_is_started_routed_and_answered () =
  let f = fixture () in
  act f (Connect { peer });
  check_true "one dial asked"
    (match f.record.asked with [ Dial (p, _) ] -> p = peer | _ -> false);
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  let connection, wire, sent = last_request f in
  Alcotest.check (Alcotest.testable pp_req ( = )) "sent" (request 3 0) sent;
  Alcotest.(check int) "one route" 1 (Correlation_map.cardinal (state f).routes);

  let response = Some (answer 3 0) in
  let message = N.Response { wire; response } in
  callback f (N.Received { peer; connection; message });
  check_outcome "answered" (Some (Answered response)) (outcome f 0);
  check_true "no route" (Correlation_map.is_empty (state f).routes);
  check_outputs "outputs" [ "ready 7"; "started 7"; "answered 7" ] f

let a_stale_callback_cannot_touch_the_connection_that_replaced_it () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  callback f (N.Closed { peer; connection = Connection_id 1 });
  opened f 2;
  act f (Fetch { peer; key = 3 });
  let connection, wire, _ = last_request f in
  Alcotest.check connection_ids "on 2" [ Connection_id 2 ] [ connection ];

  (* The old connection's answer, close and failure, delivered late. *)
  let stale = Connection_id 1 in
  let message = N.Response { wire; response = Some (answer 3 99) } in
  callback f (N.Received { peer; connection = stale; message });
  callback f (N.Closed { peer; connection = stale });
  callback f (N.Send_failed { peer; connection = stale; sent = Sent_request wire });
  check_outcome "still out" None (outcome f 0);
  check_connections "connections" [ 2 ] (connections f)

(* Section 26.10: a connection the peer opens is added beside the one in
   use, which keeps its request; a new request goes on the one established
   last. (Before, the newest number replaced the other, aborting the request
   out on it.) A connection told again is inert. *)
let a_connection_the_peer_opens_is_added_beside_the_one_in_use () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 2;
  act f (Fetch { peer; key = 3 });
  opened f 1;
  check_connections "both" [ 1; 2 ] (connections f);
  check_outcome "still out" None (outcome f 0);
  dialed f 2;
  opened f 1;
  Alcotest.check connection_ids "no close" [] (closes f);
  check_outputs "outputs" [ "ready 7"; "started 7" ] f;

  let connection, wire, _ = last_request f in
  Alcotest.check connection_ids "on 2" [ Connection_id 2 ] [ connection ];
  let message = N.Response { wire; response = Some (answer 3 0) } in
  callback f (N.Received { peer; connection; message });
  act f (Fetch { peer; key = 4 });
  let connection, _, _ = last_request f in
  Alcotest.check connection_ids "established last" [ Connection_id 1 ] [ connection ]

(* Regression: two peers dialing each other at once, each numbering its own
   connections. Each kept the one its numbers called newest, a different
   one, and closed the other: both died. Each now keeps both. *)
let simultaneous_dial inbound_first =
  let a = fixture () and b = fixture () in
  act a (Connect { peer });
  act b (Connect { peer });
  (* X is a's dial and b's inbound; Y is b's dial and a's inbound. a numbers
     them X 1 and Y 2, b numbers Y 1 and X 2. *)
  if inbound_first then (
    opened a 2;
    opened b 2;
    dialed a 1;
    dialed b 1)
  else (
    dialed a 1;
    dialed b 1;
    opened a 2;
    opened b 2);
  List.iter
    (fun end_ ->
      check_connections "both" [ 1; 2 ] (connections end_);
      Alcotest.check connection_ids "no close" [] (closes end_))
    [ a; b ]

let a_simultaneous_dial_keeps_both_connections_inbound_first () = simultaneous_dial true
let a_simultaneous_dial_keeps_both_connections_dial_first () = simultaneous_dial false

(* Regression: the dial waited for, numbered when it started, was refused
   because a connection the peer opened meanwhile had a greater number. *)
let the_dial_waited_for_is_used_whatever_its_number () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  closed f 2;
  dialed f 1;
  check_connections "connections" [ 1 ] (connections f);
  Alcotest.check connection_ids "no close" [] (closes f);
  check_outputs "outputs" [ "ready 7"; "gone 7"; "ready 7" ] f

let two_dials f =
  match dials f with
  | [ first; second ] -> (first, second)
  | dials -> Alcotest.failf "two dials: %d" (List.length dials)

let one_dial f =
  match dials f with
  | [ dial ] -> dial
  | dials -> Alcotest.failf "one dial: %d" (List.length dials)

(* Regression: [Dial_failed] named no dial, so a cancelled dial's failure
   ended the dial that replaced it. *)
let a_cancelled_dial_s_failure_cannot_end_its_replacement () =
  let f = fixture () in
  act f (Connect { peer });
  act f (Disconnect { peer });
  act f (Connect { peer });
  let cancelled, current = two_dials f in
  check_true "two dials" (not (Dial_id.equal cancelled current));
  callback f (N.Dial_failed { peer; dial = cancelled });
  check_true "dialing" (N.is_dialing (state f).p2p peer);
  check_outputs "only the disconnection's" [ "gone 7" ] f;
  dialed f 1;
  check_connections "connections" [ 1 ] (connections f);

  (* Its own failure, once answered, is stale too. *)
  callback f (N.Dial_failed { peer; dial = current });
  check_connections "connections" [ 1 ] (connections f)

(* Regression: a cancelled dial's connection made the peer ready, though
   the node had disconnected it. *)
let a_cancelled_dial_s_connection_is_closed_not_used () =
  let f = fixture () in
  act f (Connect { peer });
  act f (Disconnect { peer });
  let cancelled = List.hd (dials f) in
  connected f 1 (Some cancelled);
  check_connections "none" [] (connections f);
  check_connections "closed" [ 1 ] (closes f);

  act f (Connect { peer });
  connected f 2 (Some cancelled);
  check_true "dialing" (N.is_dialing (state f).p2p peer);
  check_connections "closed" [ 1; 2 ] (closes f);
  dialed f 3;
  check_connections "connections" [ 3 ] (connections f);
  check_outputs "outputs" [ "gone 7"; "ready 7" ] f

(* A connection the peer opens while a dial is out is used, the dial's
   answer is added beside it, and when the first goes, its request is
   aborted while the peer stays ready on the other. *)
let a_request_ends_with_its_own_connection_while_another_stays () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 1;
  act f (Fetch { peer; key = 3 });
  dialed f 2;
  check_connections "both" [ 1; 2 ] (connections f);
  check_outcome "still out" None (outcome f 0);
  closed f 1;
  check_outcome "aborted" (Some Aborted) (outcome f 0);
  check_connections "connections" [ 2 ] (connections f);
  check_outputs "outputs" [ "ready 7"; "started 7"; "aborted 7" ] f;
  act f (Fetch { peer; key = 4 });
  let connection, _, _ = last_request f in
  Alcotest.check connection_ids "on 2" [ Connection_id 2 ] [ connection ]

(* An answer comes on the connection its request went on: the same wire ID
   on another connection answers nothing. *)
let an_answer_on_another_connection_is_inert () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  opened f 2;
  act f (Fetch { peer; key = 3 });
  let connection, wire, _ = last_request f in
  Alcotest.check connection_ids "on 2" [ Connection_id 2 ] [ connection ];
  let other = N.Response { wire; response = Some (answer 3 99) } in
  callback f (N.Received { peer; connection = Connection_id 1; message = other });
  check_outcome "still out" None (outcome f 0);
  let own = N.Response { wire; response = Some (answer 3 0) } in
  callback f (N.Received { peer; connection; message = own });
  check_outcome "answered" (Some (Answered (Some (answer 3 0)))) (outcome f 0)

(* A peer cannot open connections without bound. *)
let a_connection_past_the_limit_is_closed () =
  let f = fixture () in
  for connection = 1 to max_connections do
    opened f connection
  done;
  let past = max_connections + 1 in
  opened f past;
  Alcotest.(check int) "at the limit" max_connections (List.length (connections f));
  check_connections "closed" [ past ] (closes f)

let a_send_that_fails_at_once_ends_its_request_once_after_it_started () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  let _, wire, _ = last_request f in
  callback f
    (N.Send_failed { peer; connection = Connection_id 1; sent = Sent_request wire });
  check_outcome "failed" (Some Failed) (outcome f 0);
  check_outputs "outputs" [ "ready 7"; "started 7"; "failed 7"; "gone 7" ] f;
  (* Its timeout, later, changes nothing. *)
  after f rpc_timeout;
  check_outcome "failed" (Some Failed) (outcome f 0);
  Alcotest.(check int) "no more outputs" 4 (List.length (outputs f))

let inbound f connection wire key =
  let message = N.Request { wire = Wire_id wire; request = request key wire } in
  callback f (N.Received { peer; connection = Connection_id connection; message })

(* Regression: a failed send of an answer to an inbound request was reported
   as the outbound request's own send failing. *)
let a_failed_answer_is_not_the_request_s_failure () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  (* Answered at once: a second send on the connection. *)
  inbound f 1 0 3;
  callback f
    (N.Send_failed
       { peer; connection = Connection_id 1; sent = Sent_response (Wire_id 0) });
  check_outcome "aborted" (Some Aborted) (outcome f 0)

let a_disconnect_reports_the_aborted_request_before_the_peer_gone () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  act f (Disconnect { peer });
  check_outcome "aborted" (Some Aborted) (outcome f 0);
  check_outputs "outputs" [ "ready 7"; "started 7"; "aborted 7"; "gone 7" ] f;
  check_true "closed" (List.mem (Close (peer, Connection_id 1)) f.record.asked)

let a_request_times_out () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  after f rpc_timeout;
  check_outcome "timed out" (Some Timed_out) (outcome f 0)

let one_request_per_peer_and_none_to_a_peer_not_ready () =
  let f = fixture () in
  act f (Fetch { peer; key = 3 });
  check_outcome "not ready" (Some Refused) (outcome f 0);
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  act f (Fetch { peer; key = 4 });
  check_outcome "out" None (outcome f 1);
  check_outcome "one at a time" (Some Refused) (outcome f 2)

let an_inbound_request_is_answered_at_once_or_later () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  inbound f 1 0 3;
  let mine key tag = Some (resp_of me (request key tag)) in
  Alcotest.(check (list response_sent))
    "answered at once"
    [ (Connection_id 1, Wire_id 0, mine 3 0) ]
    (responses_sent f);

  inbound f 1 1 30;
  Alcotest.(check int) "one token" 1 (tokens f);
  reply_served f (take_serve f);
  Alcotest.check response_sent "answered later"
    (Connection_id 1, Wire_id 1, mine 30 1)
    (List.nth (responses_sent f) 1);
  Alcotest.(check int) "no token" 0 (tokens f)

let an_answer_that_never_comes_expires_and_frees_the_peer_s_capacity () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  for wire = 0 to 5 do
    inbound f 1 wire 30
  done;
  Alcotest.(check int) "the sixth is past capacity" 5 (List.length f.record.serves);
  after f serving_timeout;
  Alcotest.(check int) "no token" 0 (tokens f);
  Alcotest.(check int) "none pending" 0 (N.inbound_count (state f).p2p);

  (* A late answer is inert, and the capacity is back. *)
  reply_served f (take_serve f);
  Alcotest.(check (list response_sent)) "nothing sent" [] (responses_sent f);
  inbound f 1 6 30;
  Alcotest.(check int) "one token" 1 (tokens f)

let an_answer_for_a_request_before_a_reconnect_cannot_answer_its_wire_id_reused () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  inbound f 1 0 30;
  callback f (N.Closed { peer; connection = Connection_id 1 });
  opened f 2;
  inbound f 2 0 31;

  reply_served f (take_serve f);
  Alcotest.(check (list response_sent)) "nothing sent" [] (responses_sent f);
  Alcotest.(check int) "one token" 1 (tokens f)

let the_node_s_policy_refuses_its_intents_and_lets_cleanup_through () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  let off = { fetch = false; serve = false } in
  act f (Set_policy { policy = off });

  act f (Fetch { peer; key = 4 });
  check_outcome "refused" (Some Refused) (outcome f 1);
  inbound f 1 0 3;
  Alcotest.(check (list response_sent)) "serving is off" [] (responses_sent f);

  (* The request already out still times out, and the inbound expires. *)
  after f serving_timeout;
  check_outcome "timed out" (Some Timed_out) (outcome f 0);
  Alcotest.(check int) "none pending" 0 (N.inbound_count (state f).p2p)

(* Provenance: an action lowered from a P2P Output names, in the trace, the
   mount that emitted it and the execution that ran P2P's transition. *)
let a_lowered_output_names_the_p2p_execution_that_made_it () =
  let f = fixture () in
  let log = Observe.Trace_log.create () in
  Store.add_observer (Harness.store f.node) (Observe.Trace_log.observer log);
  act f (Connect { peer });
  dialed f 1;
  act f (Fetch { peer; key = 3 });
  let connection, wire, _ = last_request f in
  let message = N.Response { wire; response = Some (answer 3 0) } in
  callback f (N.Received { peer; connection; message });

  let p2p = Protocol.instance_of f.p2p in
  let entries = Observe.Trace_log.entries log in
  let lowered =
    List.filter
      (fun (entry : Observe.Trace_entry.t) ->
        String.ends_with ~suffix:"from_p2p" entry.kind)
      entries
  in
  Alcotest.(check int) "ready, started, answered" 3 (List.length lowered);
  let origin (entry : Observe.Trace_entry.t) =
    match entry.origin with
    | Some origin -> origin
    | None -> Alcotest.fail "a lowered action has an origin"
  in
  List.iter
    (fun entry ->
      let origin = origin entry in
      Alcotest.(check int64) "lowered from P2P" p2p origin.component;
      Alcotest.(check int) "lowering ordinal" 0 origin.lowering_ordinal)
    lowered;
  (* The answer was lowered from P2P's own execution of the transport's
     callback, at its path. *)
  let answered = (origin (List.nth lowered 2)).causal in
  let cause =
    List.find (fun (entry : Observe.Trace_entry.t) -> entry.id = answered) entries
  in
  Alcotest.(check string) "path" "/p2p" cause.path;
  Alcotest.(check string) "kind" "p2p.received" cause.kind

(* [max_inbound] is the peer's, over all its connections. *)
let inbound_capacity_is_the_peer_s_over_its_connections () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  opened f 2;
  for wire = 0 to 2 do
    inbound f 1 wire 30;
    inbound f 2 wire 30
  done;
  Alcotest.(check int) "the sixth is past capacity" 5 (List.length f.record.serves);
  Alcotest.(check int) "pending" 5 (N.inbound_count (state f).p2p)

(* An inbound request whose connection goes while another stays is dropped,
   and its late answer is inert; the peer stays ready. *)
let an_inbound_request_is_dropped_with_its_connection () =
  let f = fixture () in
  act f (Connect { peer });
  dialed f 1;
  opened f 2;
  inbound f 1 0 30;
  Alcotest.(check int) "one token" 1 (tokens f);
  closed f 1;
  Alcotest.(check int) "no token" 0 (tokens f);
  check_outputs "outputs" [ "ready 7"; "received 7"; "dropped 7" ] f;

  reply_served f (take_serve f);
  Alcotest.(check (list response_sent)) "nothing sent" [] (responses_sent f);
  check_connections "connections" [ 2 ] (connections f)

(* Regression: a peer whose last connection went while its dial was still
   out was reported gone again when that dial failed. *)
let a_peer_is_reported_gone_once_when_its_dial_then_fails () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  closed f 2;
  check_true "its dial is still out" (N.is_dialing (state f).p2p peer);
  let dial = List.hd (dials f) in
  callback f (N.Dial_failed { peer; dial });
  check_outputs "outputs" [ "ready 7"; "gone 7" ] f

(* Regression: the same, when the node then disconnects the peer. And a new
   dial's failure is a report of its own. *)
let a_peer_is_reported_gone_once_when_then_disconnected () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  closed f 2;
  act f (Disconnect { peer });
  check_outputs "outputs" [ "ready 7"; "gone 7" ] f;
  act f (Connect { peer });
  let dial = List.nth (dials f) 1 in
  callback f (N.Dial_failed { peer; dial });
  check_outputs "outputs" [ "ready 7"; "gone 7"; "gone 7" ] f

(* Regression: told the peer gone while P2P's own dial was still out, the
   node dialed again and was refused; when that dial failed, gone had been
   told already, so the node never heard how its connect ended. The connect
   now keeps the dial still out and renews its report: the dial's failure is
   told, and so is its connection. *)
let a_connect_after_gone_keeps_the_dial_still_out_and_reports_it () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  closed f 2;
  check_outputs "outputs" [ "ready 7"; "gone 7" ] f;
  act f (Connect { peer });
  let dial = one_dial f in
  callback f (N.Dial_failed { peer; dial });
  check_outputs "outputs" [ "ready 7"; "gone 7"; "gone 7" ] f;

  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  closed f 2;
  act f (Connect { peer });
  let dial = one_dial f in
  connected f 5 (Some dial);
  check_connections "connections" [ 5 ] (connections f);
  check_connections "no close" [] (closes f);
  check_outputs "outputs" [ "ready 7"; "gone 7"; "ready 7" ] f

(* Disconnect, then connect: the dial still out is cancelled, and a fresh
   one made; the cancelled dial's connection is closed, its failure
   inert. *)
let a_disconnect_then_a_connect_dials_afresh () =
  let f = fixture () in
  act f (Connect { peer });
  act f (Disconnect { peer });
  act f (Connect { peer });
  let cancelled, fresh = two_dials f in
  callback f (N.Dial_failed { peer; dial = cancelled });
  check_true "the fresh dial is out"
    (Option.equal Dial_id.equal (N.dialing (state f).p2p peer) (Some fresh));
  connected f 5 (Some cancelled);
  check_connections "closed" [ 5 ] (closes f);
  connected f 6 (Some fresh);
  check_connections "connections" [ 6 ] (connections f);
  check_outputs "outputs" [ "gone 7"; "ready 7" ] f

(* Regression: with a connect replacing the dial still out, two nodes whose
   parents connect again whenever told the peer is gone never settled. Each
   replaced dial's connection came, and was closed by its dialer; the other
   end took that for a lost connection, connected again, and replaced its
   own dial, whose connection then met the same fate. Every handshake
   succeeded and none was kept. Now both end connected.

   Answered in the order made, the replaced dials did end, so the cycle
   needs the order that kept it going: a dial its dialer no longer waits for
   is answered first, the newest first, as a quicker handshake would be. *)
let two_nodes_retrying_on_gone_end_connected () =
  let a = fixture () and b = fixture () in
  act a (Connect { peer });
  act b (Connect { peer });
  (* B's dial is answered first, while A's is still out; then the connection
     breaks, and both parents connect again. *)
  opened a 1;
  connected b 1 (Some (List.hd (dials b)));
  closed a 1;
  act a (Connect { peer });
  closed b 1;
  act b (Connect { peer });
  (* Every dial succeeds; a parent told the peer gone connects again. *)
  let out =
    ref
      (List.map (fun d -> (true, d)) (dials a)
      @ List.map (fun d -> (false, d)) (List.tl (dials b)))
  in
  let next = ref 10 in
  while !out <> [] do
    if !next >= 40 then
      Alcotest.failf "no end: %d and %d dials"
        (List.length (dials a))
        (List.length (dials b));
    let waits (from_a, dial) =
      let f = if from_a then a else b in
      Option.equal Dial_id.equal (N.dialing (state f).p2p peer) (Some dial)
    in
    let stale =
      List.fold_left
        (fun (i, found) entry -> (i + 1, if waits entry then found else Some i))
        (0, None) !out
      |> snd
    in
    let index = Option.value stale ~default:0 in
    let from_a, dial = List.nth !out index in
    out := List.filteri (fun i _ -> i <> index) !out;
    let dialer, other = if from_a then (a, b) else (b, a) in
    opened other !next;
    connected dialer !next (Some dial);
    if List.mem (Connection_id !next) (closes dialer) then (
      closed other !next;
      let made = List.length (dials other) in
      act other (Connect { peer });
      if List.length (dials other) > made then
        out := !out @ [ (not from_a, last (dials other)) ]);
    incr next
  done;
  check_true "both ready" (N.is_ready (state a).p2p peer && N.is_ready (state b).p2p peer);
  check_connections "a closed none" [] (closes a);
  check_connections "b closed none" [] (closes b)

(* Regression: Disconnect cancels the dial still out also when a connection
   the peer opened is held; a connect then dials afresh, and the cancelled
   dial's connection is closed. *)
let a_disconnect_holding_a_connection_cancels_the_dial_too () =
  let f = fixture () in
  act f (Connect { peer });
  opened f 2;
  check_true "its dial is out" (N.dialing (state f).p2p peer <> None);
  act f (Disconnect { peer });
  check_true "its dial is cancelled" (N.dialing (state f).p2p peer = None);
  act f (Connect { peer });
  let cancelled, fresh = two_dials f in
  connected f 5 (Some cancelled);
  check_connections "closed" [ 2; 5 ] (closes f);
  connected f 6 (Some fresh);
  check_connections "connections" [ 6 ] (connections f);
  check_outputs "outputs" [ "ready 7"; "gone 7"; "ready 7" ] f

let () =
  let test name f = Alcotest.test_case name `Quick f in
  Alcotest.run "p2p contracts"
    [
      ( "contracts",
        [
          test "a fetch is started, routed and answered"
            a_fetch_is_started_routed_and_answered;
          test "a stale callback cannot touch the connection that replaced it"
            a_stale_callback_cannot_touch_the_connection_that_replaced_it;
          test "a connection the peer opens is added beside the one in use"
            a_connection_the_peer_opens_is_added_beside_the_one_in_use;
          test "a simultaneous dial keeps both connections, inbound first"
            a_simultaneous_dial_keeps_both_connections_inbound_first;
          test "a simultaneous dial keeps both connections, dial first"
            a_simultaneous_dial_keeps_both_connections_dial_first;
          test "the dial waited for is used whatever its number"
            the_dial_waited_for_is_used_whatever_its_number;
          test "a cancelled dial's failure cannot end its replacement"
            a_cancelled_dial_s_failure_cannot_end_its_replacement;
          test "a cancelled dial's connection is closed, not used"
            a_cancelled_dial_s_connection_is_closed_not_used;
          test "a request ends with its own connection while another stays"
            a_request_ends_with_its_own_connection_while_another_stays;
          test "an answer on another connection is inert"
            an_answer_on_another_connection_is_inert;
          test "a connection past the limit is closed"
            a_connection_past_the_limit_is_closed;
          test "a send that fails at once ends its request once, after it started"
            a_send_that_fails_at_once_ends_its_request_once_after_it_started;
          test "a failed answer is not the request's failure"
            a_failed_answer_is_not_the_request_s_failure;
          test "a disconnect reports the aborted request before the peer gone"
            a_disconnect_reports_the_aborted_request_before_the_peer_gone;
          test "a request times out" a_request_times_out;
          test "one request per peer, and none to a peer not ready"
            one_request_per_peer_and_none_to_a_peer_not_ready;
          test "an inbound request is answered at once or later"
            an_inbound_request_is_answered_at_once_or_later;
          test "an answer that never comes expires and frees the peer's capacity"
            an_answer_that_never_comes_expires_and_frees_the_peer_s_capacity;
          test
            "an answer for a request before a reconnect cannot answer its wire ID reused"
            an_answer_for_a_request_before_a_reconnect_cannot_answer_its_wire_id_reused;
          test "the node's policy refuses its Intents and lets cleanup through"
            the_node_s_policy_refuses_its_intents_and_lets_cleanup_through;
          test "a lowered Output names the P2P execution that made it"
            a_lowered_output_names_the_p2p_execution_that_made_it;
          test "inbound capacity is the peer's, over its connections"
            inbound_capacity_is_the_peer_s_over_its_connections;
          test "an inbound request is dropped with its connection"
            an_inbound_request_is_dropped_with_its_connection;
          test "a peer is reported gone once when its dial then fails"
            a_peer_is_reported_gone_once_when_its_dial_then_fails;
          test "a peer is reported gone once when then disconnected"
            a_peer_is_reported_gone_once_when_then_disconnected;
          test "a connect after gone keeps the dial still out and reports it"
            a_connect_after_gone_keeps_the_dial_still_out_and_reports_it;
          test "a disconnect, then a connect, dials afresh"
            a_disconnect_then_a_connect_dials_afresh;
          test "two nodes retrying on gone end connected"
            two_nodes_retrying_on_gone_end_connected;
          test "a disconnect holding a connection cancels the dial too"
            a_disconnect_holding_a_connection_cancels_the_dial_too;
        ] );
    ]
