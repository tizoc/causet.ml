(* Port of causet-test's tests/acceptance.rs: two Stores on a simulated
   network, a reply delayed past its timeout, a restart that refuses the old
   route and leaves an old request to the application's recovery policy, and
   the same outcome from the same seed. Then a focused transition test from a
   validated fixture, a composite on the real Store, and generated runs of
   the two Stores with planted failures for shrinking to find. *)

open Causet
open Causet_test

let ms = 1_000_000
let timeout = 50 * ms
let start = 1_000 * ms

module Rmap = Map.Make (Request_id)
module Rset = Set.Make (Request_id)

(* ------------------------------------------------------------------------ *)
(* Messages *)

type client_action =
  | Ask of { n : int }
  | Answered of { request : Request_id.t; value : int }
  | Timed_out of { request : Request_id.t }

type client_effect =
  | Send of { request : Request_id.t; n : int }
  | Arm of { request : Request_id.t }

(* The server's answer, as the network delivers it. *)
type answer = { a_request : Request_id.t; a_value : int }
type serve = { s_request : Request_id.t; s_n : int }
type respond = { r_request : Request_id.t; r_value : int }

(* A client's question, as the network delivers it. *)
type question = { q_request : Request_id.t; q_n : int }

let client_actions =
  Message.make ~name:"client.action"
    ~kind:(function
      | Ask _ -> "client.action.ask"
      | Answered _ -> "client.action.answered"
      | Timed_out _ -> "client.action.timed_out")
    ()

let client_effects =
  Message.make ~name:"client.effect"
    ~kind:(function Send _ -> "client.effect.send" | Arm _ -> "client.effect.arm")
    ()

let answer_events : answer Message.t = Message.single "client.answer"

let serve_actions : serve Message.t =
  Message.make ~name:"server.action" ~kind:(fun _ -> "server.serve") ()

let respond_effects : respond Message.t =
  Message.make ~name:"server.effect" ~kind:(fun _ -> "server.respond") ()

let question_events : question Message.t = Message.single "server.question"

(* ------------------------------------------------------------------------ *)
(* The client's State *)

type client_state = {
  session : session_id;  (** This run's session: the requests it makes carry it. *)
  pending : int Rmap.t;
  answered : (int * int) list;
  timed_out : int list;
  abandoned : int list;  (** Requests of an earlier run, given up on recovery. *)
  settled : Rset.t;
      (** Every request that ended (answered, timed out or given up): each ends once. *)
  stale_answers : int;  (** Answers for no pending request: late, or given up. *)
  stale_timeouts : int;  (** Timeouts for a request already answered. *)
}

let client_state session =
  {
    session;
    pending = Rmap.empty;
    answered = [];
    timed_out = [];
    abandoned = [];
    settled = Rset.empty;
    stale_answers = 0;
    stale_timeouts = 0;
  }

(* The application's recovery policy: the old run's requests are given up,
   so an answer to one, however it arrives, finds nothing pending. *)
let recovered s session =
  {
    s with
    abandoned = s.abandoned @ List.map snd (Rmap.bindings s.pending);
    settled = Rmap.fold (fun r _ acc -> Rset.add r acc) s.pending s.settled;
    pending = Rmap.empty;
    session;
  }

(* Requests made in every run so far: pending or ended. *)
let made s = Rmap.cardinal s.pending + Rset.cardinal s.settled

let invariants s =
  let pending = Rmap.bindings s.pending in
  match List.find_opt (fun ((r : Request_id.t), _) -> r.session <> s.session) pending with
  | Some (r, _) ->
      Error (Format.asprintf "%a is pending from another run" Request_id.pp r)
  | None -> (
      match List.find_opt (fun (n, value) -> value <> n * n) s.answered with
      | Some (n, value) -> Error (Printf.sprintf "%d answered %d" n value)
      | None -> (
          match List.find_opt (fun (r, _) -> Rset.mem r s.settled) pending with
          | Some (r, _) ->
              Error (Format.asprintf "%a is both pending and ended" Request_id.pp r)
          | None ->
              let ended =
                List.length s.answered + List.length s.timed_out + List.length s.abandoned
              in
              if ended <> Rset.cardinal s.settled then
                Error
                  (Printf.sprintf "%d endings for %d requests: one ended twice" ended
                     (Rset.cardinal s.settled))
              else Ok ()))

(* A comparable form: maps and sets as their ordered elements. *)
let client_repr s =
  ( s.session,
    Rmap.bindings s.pending,
    s.answered,
    s.timed_out,
    s.abandoned,
    Rset.elements s.settled,
    s.stale_answers,
    s.stale_timeouts )

type server_state = { served : int }

(* ------------------------------------------------------------------------ *)
(* The network: the application's own model. A message travels for a seeded
   delay and is delivered to the address it was sent to, as a network thread
   would deliver it: into the worker lane. If that address is gone (the
   client restarted), the delivery is refused, and the network resends to
   the client's current address. *)

type client_mount =
  (client_state, client_services, client_action, client_effect, never) mount

and server_mount = (server_state, server_services, serve, respond, never) mount
and client_services = { net : net; timer : Timer.t }
and server_services = { snet : net }

and net = {
  clock : Clock.Manual.m;
  rng : Rng.t;
  mutable seq : int;
  mutable in_flight : flight list;
  mutable client : (Sink.t * client_mount) option;
  mutable server : (Sink.t * server_mount) option;
  mutable slow_next_answer : int option;
      (** Extra delay for the next answer: the test's lever. *)
  mutable refused : int;  (** Deliveries refused at an old address. *)
  mutable rerouted : int;  (** Resent to the current address after a refusal. *)
}

and flight = { at : timestamp; fseq : int; message : net_message }

and net_message =
  | Question_msg of { request : Request_id.t; n : int; to_ : Sink.t * server_mount }
  | Answer_msg of { request : Request_id.t; value : int; to_ : Sink.t * client_mount }

(* A network whose delays are drawn from [rng]. *)
let net_create rng clock =
  {
    clock;
    rng;
    seq = 0;
    in_flight = [];
    client = None;
    server = None;
    slow_next_answer = None;
    refused = 0;
    rerouted = 0;
  }

let counters net = (net.refused, net.rerouted)

let net_send net message extra =
  let delay = Rng.between net.rng ms (5 * ms) + extra in
  let at = Clock.Manual.get net.clock + delay in
  net.in_flight <-
    List.stable_sort
      (fun a b -> compare (a.at, a.fseq) (b.at, b.fseq))
      (net.in_flight @ [ { at; fseq = net.seq; message } ]);
  net.seq <- net.seq + 1

let net_ask net request n =
  let to_ = Option.get net.server in
  net_send net (Question_msg { request; n; to_ }) 0

let net_answer net request value =
  let to_ = Option.get net.client in
  let extra = Option.value net.slow_next_answer ~default:0 in
  net.slow_next_answer <- None;
  net_send net (Answer_msg { request; value; to_ }) extra

let world net =
  {
    next_time = (fun () -> match net.in_flight with f :: _ -> Some f.at | [] -> None);
    advance =
      (fun now ->
        let due, later = List.partition (fun f -> f.at <= now) net.in_flight in
        net.in_flight <- later;
        List.iter
          (fun f ->
            match f.message with
            | Question_msg { request; n; to_ = sink, mount } -> (
                match
                  Sink.try_send_to_as_worker sink mount question_events
                    { q_request = request; q_n = n }
                with
                | Ok () -> ()
                | Error _ -> failwith "the server is up")
            | Answer_msg { request; value; to_ = sink, mount } -> (
                match
                  Sink.try_send_to_as_worker sink mount answer_events
                    { a_request = request; a_value = value }
                with
                | Ok () -> ()
                | Error (Closed answer) -> (
                    (* The old route is refused; a valid new route carries
                       the same old request. *)
                    net.refused <- net.refused + 1;
                    let sink, mount = Option.get net.client in
                    match Sink.try_send_to_as_worker sink mount answer_events answer with
                    | Ok () -> net.rerouted <- net.rerouted + 1
                    | Error _ -> failwith "the current client is up")
                | Error _ -> failwith "unexpected send error"))
          due);
  }

(* ------------------------------------------------------------------------ *)
(* The client: asks for squares, times each request out. *)

let client_comp =
  Component.make ~name:"client" ~actions:client_actions ~effects:client_effects
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Ask { n } ->
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s -> { s with pending = Rmap.add request n s.pending });
          Ctx.emit_effect ctx (Send { request; n });
          Ctx.emit_effect ctx (Arm { request })
      | Answered { request; value } ->
          Ctx.update ctx (fun s ->
              match Rmap.find_opt request s.pending with
              | Some n ->
                  {
                    s with
                    pending = Rmap.remove request s.pending;
                    answered = s.answered @ [ (n, value) ];
                    settled = Rset.add request s.settled;
                  }
              | None -> { s with stale_answers = s.stale_answers + 1 })
      | Timed_out { request } ->
          Ctx.update ctx (fun s ->
              match Rmap.find_opt request s.pending with
              | Some n ->
                  {
                    s with
                    pending = Rmap.remove request s.pending;
                    timed_out = s.timed_out @ [ n ];
                    settled = Rset.add request s.settled;
                  }
              | None -> { s with stale_timeouts = s.stale_timeouts + 1 }))
    ~handle:(fun ctx eff _ ->
      let services = Effect_ctx.service ctx in
      match eff with
      | Send { request; n } -> net_ask services.net request n
      | Arm { request } ->
          let reply = Effect_ctx.reply ctx Timer.fired in
          Timer.after services.timer ~delay:timeout reply request)
    ~register:(fun r ->
      Registrar.event r answer_events (fun a port ->
          Port.emit_action port (Answered { request = a.a_request; value = a.a_value }));
      Registrar.event r Timer.fired (fun (f : Timer.fired) port ->
          Port.emit_action port (Timed_out { request = f.request })))
    ()

(* ------------------------------------------------------------------------ *)
(* The server: squares. *)

let server_comp =
  Component.make ~name:"server" ~actions:serve_actions ~effects:respond_effects
    ~outputs:Message.never
    ~reduce:(fun ctx { s_request; s_n } _ ->
      Ctx.update ctx (fun s -> { served = s.served + 1 });
      Ctx.emit_effect ctx { r_request = s_request; r_value = s_n * s_n })
    ~handle:(fun ctx { r_request; r_value } _ ->
      net_answer (Effect_ctx.service ctx).snet r_request r_value)
    ~register:(fun r ->
      Registrar.event r question_events (fun q port ->
          Port.emit_action port { s_request = q.q_request; s_n = q.q_n }))
    ()

let attach_client net node =
  net.client <- Some (Node.sink node, Node.mount node (Path.root client_comp))

let attach_server net node =
  net.server <- Some (Node.sink node, Node.mount node (Path.root server_comp))

(* ------------------------------------------------------------------------ *)
(* The scenario *)

let client_node (scenario : Scenario.t) net state =
  let timer, control = Timer.simulated (Scenario.clock scenario) in
  let node =
    Node.create ~name:"client" client_comp ~state ~services:{ net; timer }
      ~session:state.session (Scenario.clock scenario)
  in
  Node.check (Node.with_timer node control) "client" invariants

(* The client restarted with the State its recovery policy keeps. *)
let restart client (scenario : Scenario.t) net session =
  let timer, control = Timer.simulated (Scenario.clock scenario) in
  let kept = recovered (Node.state client) session in
  Node.restart client ~state:kept ~services:{ net; timer } ~session;
  ignore (Node.with_timer client control);
  attach_client net client

let server_node (scenario : Scenario.t) net =
  Node.create ~name:"server" server_comp ~state:{ served = 0 } ~services:{ snet = net }
    ~session:2 (Scenario.clock scenario)

let stop_string = function
  | Reached -> "Reached"
  | Quiet -> "Quiet"
  | Budget -> "Budget"
  | Time_limit -> "Time_limit"

(* Runs until nothing is held or ahead; fails on a stop of any other kind. *)
let settle scenario nodes net =
  let w = world net in
  let stop = Scenario.run_until scenario nodes w ~steps:1_000 ~until:(fun () -> false) in
  Alcotest.(check string) (Scenario.pending scenario nodes w) "Quiet" (stop_string stop)

let ask client n =
  let mount = Node.mount client (Path.root client_comp) in
  ignore (Node.dispatch client mount (Ask { n }))

type outcome = {
  log : string list;
  client_s : client_state;
  server_s : server_state;
  network : int * int;  (** (refused at the old address, resent to the new one) *)
}

let run seed =
  let scenario = Scenario.create seed start in
  let net = net_create (Rng.create seed) (Scenario.clock scenario) in
  let client = client_node scenario net (client_state 1) in
  let server = server_node scenario net in
  attach_client net client;
  attach_server net server;
  let nodes = [ driven client; driven server ] in

  (* 1. Answered in time. *)
  ask client 1;
  settle scenario nodes net;
  Alcotest.(check (list (pair int int)))
    "answered"
    [ (1, 1) ]
    (Node.state client).answered;
  (* Settling ran on to the timeout, which found the request answered. *)
  Alcotest.(check int) "stale timeouts" 1 (Node.state client).stale_timeouts;

  (* 2. The answer comes after the timeout: the timeout wins, the late
     answer is ignored. *)
  net.slow_next_answer <- Some (2 * timeout);
  ask client 2;
  settle scenario nodes net;
  Alcotest.(check (list int)) "timed out" [ 2 ] (Node.state client).timed_out;
  Alcotest.(check int) "stale answers" 1 (Node.state client).stale_answers;

  (* 3. The client restarts while the answer to 3 is in flight to its old
     address. *)
  ask client 3;
  let w = world net in
  let stop =
    Scenario.run_until scenario nodes w ~steps:100 ~until:(fun () ->
        (Node.state server).served = 3)
  in
  Alcotest.(check string) (Scenario.pending scenario nodes w) "Reached" (stop_string stop);
  restart client scenario net 3;
  settle scenario nodes net;
  (* The old route refused the answer; through the new route it reached a
     run that gave the request up, so it was ignored. *)
  Alcotest.(check (pair int int)) "network" (1, 1) (counters net);
  Alcotest.(check (list int)) "abandoned" [ 3 ] (Node.state client).abandoned;
  Alcotest.(check int) "stale answers" 2 (Node.state client).stale_answers;

  (* 4. The new run works. *)
  ask client 4;
  settle scenario nodes net;
  Alcotest.(check (list (pair int int)))
    "answered"
    [ (1, 1); (4, 16) ]
    (Node.state client).answered;
  Alcotest.(check int) "stale timeouts" 2 (Node.state client).stale_timeouts;

  (* An invariant across both Stores: every question reached the server
     once. *)
  Alcotest.(check int) "served" 4 (Node.state server).served;
  Alcotest.(check (result unit string))
    "invariants" (Ok ())
    (invariants (Node.state client));
  {
    log = Scenario.log scenario;
    client_s = Node.state client;
    server_s = Node.state server;
    network = counters net;
  }

let outcome_repr o = (o.log, client_repr o.client_s, o.server_s, o.network)

let the_scenario_repeats_from_its_seed () =
  let first = run 7 in
  let second = run 7 in
  Alcotest.(check bool)
    "the same seed made different decisions" true
    (outcome_repr first = outcome_repr second)

let every_seed_reaches_the_same_outcome_by_its_own_timing () =
  (* Request IDs carry their execution's time, which differs by seed: the
     requests that ended are compared by count. *)
  let untimed o =
    let settled = Rset.cardinal o.client_s.settled in
    (client_repr { o.client_s with settled = Rset.empty }, settled, o.server_s, o.network)
  in
  let outcomes = List.map run [ 1; 2; 3; 4; 5 ] in
  let first = List.hd outcomes in
  List.iter
    (fun o -> Alcotest.(check bool) "the same outcome" true (untimed o = untimed first))
    (List.tl outcomes);
  (* The network's delays differ by seed, and so do the steps' times. *)
  Alcotest.(check bool)
    "logs differ" true
    (List.exists (fun o -> o.log <> first.log) outcomes)

(* ------------------------------------------------------------------------ *)
(* A focused transition test from a validated fixture: a client mid-request,
   whose pending request, armed timer and session agree with its State. *)

(* Builds a client State with requests already pending, and checks the
   domain's invariants before any test uses it. *)
type fixture = { f_state : client_state; next_index : int }

let fixture session = { f_state = client_state session; next_index = 0 }

(* A request made by execution [execution] of this run. *)
let fixture_pending f execution n =
  let request =
    { Request_id.session = f.f_state.session; execution; index = f.next_index }
  in
  ( {
      f_state = { f.f_state with pending = Rmap.add request n f.f_state.pending };
      next_index = f.next_index + 1;
    },
    request )

let fixture_build f = Result.map (fun () -> f.f_state) (invariants f.f_state)
let lane_string = function Store_lane -> "Store" | Worker_lane -> "Worker"

let only what = function
  | [ e ] -> e
  | l -> Alcotest.failf "%s: %d held" what (List.length l)

let a_transition_from_a_fixture_mid_request () =
  let clock = Clock.Manual.create start in
  let session = 10 in
  let made_by = start - 1 in
  let fixture, request = fixture_pending (fixture session) made_by 7 in
  let state =
    match fixture_build fixture with
    | Ok s -> s
    | Error e -> Alcotest.failf "a valid fixture: %s" e
  in
  let net = net_create (Rng.create 0) clock in
  let timer, control = Timer.simulated clock in
  let client =
    Node.check
      (Node.with_timer
         (Node.create ~name:"client" client_comp ~state ~services:{ net; timer } ~session
            clock)
         control)
      "client" invariants
  in
  let mount = Node.mount client (Path.root client_comp) in

  (* The Service work the fixture implies: the request's timeout is armed,
     answering through a reply route of the Effect that armed it. *)
  let reply =
    match Sink.reply_for (Node.sink client) mount Timer.fired ~answers:made_by with
    | Some r -> r
    | None -> Alcotest.fail "a registered Event"
  in
  (* The fixture arms it on a simulated Timer of its own. *)
  let fixture_timer, fixture_control = Timer.simulated clock in
  Timer.after fixture_timer ~delay:timeout reply request;

  (* The transition under test: the answer arrives. *)
  (match
     Sink.try_send_to_as_worker (Node.sink client) mount answer_events
       { a_request = request; a_value = 49 }
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the answer was refused");
  let held = only "one held Event" (Node.held client) in
  Alcotest.(check (pair (pair string string) (pair int string)))
    "held"
    (("client.answer", "/"), (session, "Worker"))
    ((held.held_kind, held.held_path), (held.held_session, lane_string held.held_lane));
  Node.deliver client held.held_id;
  Alcotest.(check (list (pair int int)))
    "answered"
    [ (7, 49) ]
    (Node.state client).answered;
  Alcotest.(check bool) "nothing pending" true (Rmap.is_empty (Node.state client).pending);

  (* Later, the timeout fires for a request no longer pending: ignored. *)
  Clock.Manual.advance clock timeout;
  Alcotest.(check int) "fired" 1 (Timer.Control.fire_due fixture_control);
  let fired = only "the timeout" (Node.held client) in
  Node.deliver client fired.held_id;
  Alcotest.(check int) "stale timeouts" 1 (Node.state client).stale_timeouts;
  Alcotest.(check (list int)) "timed out" [] (Node.state client).timed_out

(* ------------------------------------------------------------------------ *)
(* A composite on the real Store: the guide's shop, its cart and payment
   children and its gateway all real, the gateway's answer held until
   delivered. *)

let a_composite_runs_its_real_children () =
  let open Causet_guide.Shop in
  let clock = Clock.Manual.create start in
  let shop =
    Node.check
      (Node.of_protocol ~name:"shop" (protocol ()) ~state:empty
         ~services:{ gateway = approve_up_to 100 }
         ~session:20 clock)
      "paid within the gateway's limit"
      (fun s ->
        if List.for_all (fun p -> p <= 100) s.payments.paid then Ok ()
        else
          Error
            (Printf.sprintf "paid [%s]"
               (String.concat "; " (List.map string_of_int s.payments.paid))))
  in
  let cart = Node.mount shop (Path.child (Path.root shop_root) cart_slot) in
  ignore (Node.dispatch shop cart (Add { item = "tea"; price = 4 }));
  ignore (Node.dispatch shop cart Checkout);

  (* The cart's Output, the root's Pay and the payments child's Charge all
     ran; the gateway answered at once, and its answer is held. *)
  Alcotest.(check int) "orders" 1 (Node.state shop).orders;
  Alcotest.(check int) "pending" 1 (Pending.cardinal (Node.state shop).payments.pending);
  let charged = only "the gateway's answer" (Node.held shop) in
  Alcotest.(check (pair string string))
    "held"
    ("payments.charged", "/payments")
    (charged.held_kind, charged.held_path);
  Alcotest.(check (option bool))
    "approved" (Some true)
    (Option.map
       (fun (c : charged) -> c.approved)
       (Node.inspect shop charged.held_id charged_message));
  Node.deliver shop charged.held_id;
  Alcotest.(check (list int)) "paid" [ 4 ] (Node.state shop).payments.paid;
  let scenario = Scenario.create 0 start in
  Alcotest.(check string)
    "quiet" "Quiet"
    (stop_string
       (Scenario.run_until scenario
          [ driven shop ]
          no_world ~steps:10
          ~until:(fun () -> false)))

(* ------------------------------------------------------------------------ *)
(* Generated runs: the same two Stores, driven by steps drawn from one stream
   of choices, which also sets the network's delays. *)

(* A failure planted in a generated run, for shrinking to find. *)
type planted =
  | No_plant
  | Never_times_out
      (** The client claims it never times out, which a slow enough answer breaks. *)
  | Served_every_question
      (** A progress property posing as an invariant: the server served every question
          asked, which fails while one is in flight. *)

(* One generated run, with [planted] failures. *)
let generated rng planted =
  let scenario = Scenario.with_rng rng start in
  let net = net_create rng (Scenario.clock scenario) in

  (* Across both Stores, while work is in flight and the client restarts:
     the server never served more questions than the client has made. *)
  let pair = Group.create "pair" in
  Group.check pair "served at most what was asked" (fun views _ ->
      match (Group.view views "client", Group.view views "server") with
      | Some made, Some served when served > made ->
          Error (Printf.sprintf "served %d of %d" served made)
      | _ -> Ok ());
  if planted = Served_every_question then
    Group.check pair "served every question" (fun views _ ->
        match (Group.view views "client", Group.view views "server") with
        | Some made, Some served when served <> made ->
            Error (Printf.sprintf "served %d of %d" served made)
        | _ -> Ok ());
  let client = Node.publish (client_node scenario net (client_state 1)) pair made in
  if planted = Never_times_out then
    ignore
      (Node.check client "never times out" (fun s ->
           match s.timed_out with
           | n :: _ -> Error (Printf.sprintf "%d timed out" n)
           | [] -> Ok ()));
  let server = Node.publish (server_node scenario net) pair (fun s -> s.served) in
  attach_client net client;
  attach_server net server;
  let nodes = [ driven client; driven server ] in
  let w = world net in
  let asks = ref 0 and session = ref 2 in
  while Rng.more rng do
    match Rng.below rng 4 with
    | 0 -> ignore (Scenario.step scenario nodes w)
    | 1 ->
        let mount = Node.mount client (Path.root client_comp) in
        ignore (Node.dispatch client mount (Ask { n = Rng.below rng 10 }));
        incr asks
    | 2 -> net.slow_next_answer <- Some (ms * Rng.below rng 100)
    | _ ->
        incr session;
        restart client scenario net !session
  done;
  let stop = Scenario.run_until scenario nodes w ~steps:10_000 ~until:(fun () -> false) in
  Alcotest.(check string) (Scenario.pending scenario nodes w) "Quiet" (stop_string stop);

  (* Across both Stores, once quiet: every question reached the server, and
     every request ended one way. *)
  let state = Node.state client in
  Alcotest.(check int) "served" !asks (Node.state server).served;
  Alcotest.(check bool) "nothing pending" true (Rmap.is_empty state.pending);
  Alcotest.(check int)
    "ended" !asks
    (List.length state.answered + List.length state.timed_out
   + List.length state.abandoned)

let generated_runs_keep_every_invariant () =
  check ~config:default (fun rng -> generated rng No_plant)

let a_planted_failure_shrinks_to_a_slow_answer_after_one_question () =
  let found =
    match search default (fun rng -> generated rng Never_times_out) with
    | Some f -> f
    | None -> Alcotest.fail "a timeout"
  in
  Alcotest.(check string) "key" "node client: invariant \"never times out\"" found.key;
  Alcotest.(check bool)
    (Harness_util.found_string found)
    true
    (List.length found.choices < found.original);
  (* What is left, choice by choice: a step (more), Ask, n = 0, the
     question's network delay (the shortest, 1 ms); a step, slow the next
     answer, by 49 ms; no more steps; the answer's own delay (1 ms). The
     answer then arrives 1 + 1 + 49 = 51 ms in, past the 50 ms timeout; by 48
     it would arrive at 50 ms, delivered before the timer that fires at the
     same time, and answered in time. *)
  Alcotest.check Harness_util.ints
    (Harness_util.found_string found)
    [ 1; 1; 0; 0; 1; 2; 49; 0; 0 ]
    found.choices;
  (* The list repeats the failure. *)
  let replayed =
    match replay found.choices (fun rng -> generated rng Never_times_out) with
    | () -> false
    | exception _ -> true
  in
  Alcotest.(check bool) "the replay fails" true replayed

let progress_posing_as_an_invariant_fails_while_a_question_is_in_flight () =
  let found =
    match search default (fun rng -> generated rng Served_every_question) with
    | Some f -> f
    | None -> Alcotest.fail "a question in flight"
  in
  Alcotest.(check string)
    "key" "group pair: invariant \"served every question\"" found.key;
  (* A step, Ask, n = 0, and the question's network delay (drawn as it is
     sent): the client has made a request the server has not seen yet. *)
  Alcotest.check Harness_util.ints
    (Harness_util.found_string found)
    [ 1; 1; 0; 0 ] found.choices

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "acceptance"
    [
      ( "scenario",
        [
          t "the scenario repeats from its seed" the_scenario_repeats_from_its_seed;
          t "every seed reaches the same outcome by its own timing"
            every_seed_reaches_the_same_outcome_by_its_own_timing;
          t "a transition from a fixture mid-request"
            a_transition_from_a_fixture_mid_request;
          t "a composite runs its real children" a_composite_runs_its_real_children;
        ] );
      ( "generated",
        [
          t "generated runs keep every invariant" generated_runs_keep_every_invariant;
          t "a planted failure shrinks to a slow answer after one question"
            a_planted_failure_shrinks_to_a_slow_answer_after_one_question;
          t "progress posing as an invariant fails while a question is in flight"
            progress_posing_as_an_invariant_fails_while_a_question_is_in_flight;
        ] );
    ]
