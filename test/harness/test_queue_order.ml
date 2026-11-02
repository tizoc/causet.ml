(* Port of causet-test's tests/queue_order.rs: the harness keeps
   production's queue order and the timer's lane and lifecycle: the default
   scenario delivers held Events in the order the lanes would run them, a
   simulated timer publishes into the worker lane like the threaded one, and
   a stopped (Rust: dropped) Timer fires nothing. A simulated Service that
   stands in for a thread publishes as one too. *)

open Causet
open Causet_test

type input = Prime | Note of string

let inputs =
  Message.make ~name:"edge.action"
    ~kind:(function Prime -> "edge.action.prime" | Note _ -> "edge.action.note")
    ()

type seen = Seen of string

let seen_events = Message.single "edge.seen"

let probe =
  Component.make ~name:"probe" ~actions:inputs ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx input _ ->
      match input with Note note -> Ctx.update ctx (fun s -> s @ [ note ]) | Prime -> ())
    ~register:(fun r ->
      Registrar.event r seen_events (fun (Seen note) port ->
          Port.emit_action port (Note note));
      Registrar.event r Timer.fired (fun _ port -> Port.emit_action port (Note "timer")))
    ()

let strings = Alcotest.(list string)
let lane_string = function Store_lane -> "Store" | Worker_lane -> "Worker"
let lanes held = List.map (fun e -> lane_string e.held_lane) held

let node (scenario : Scenario.t) =
  let node =
    Node.create ~name:"probe" probe ~state:[] ~services:() ~session:1
      (Scenario.clock scenario)
  in
  let mount = Node.mount node (Path.root probe) in
  ignore (Node.dispatch node mount Prime);
  (node, mount)

let ok = function Ok () -> () | Error _ -> Alcotest.fail "the send was refused"

(* Sends from a thread of its own: the worker lane. *)
let from_worker sink mount =
  Thread.join
    (Thread.create
       (fun () -> ok (Sink.try_send_to sink mount seen_events (Seen "worker")))
       ())

let production_store capacity =
  let reg = Registry.create () in
  Registry.root reg probe;
  let proto = match Registry.freeze reg with Ok p -> p | Error _ -> assert false in
  let mount = Protocol.resolve_exn proto (Path.root probe) in
  let sink, inbox = Event_bus.create proto ~worker_capacity:capacity in
  let store = Store.create_exn proto ~state:[] ~services:() inbox in
  (store, sink, mount)

let step_some s nodes world =
  match Scenario.step s nodes world with
  | Some _ -> ()
  | None -> Alcotest.fail "nothing to step"

let default_scenario_preserves_store_lane_priority () =
  (* First establish the production order for exactly this backlog. *)
  let production, sink, mount = production_store 8 in
  ignore (Store.dispatch_exn production mount Prime);
  from_worker sink mount;
  ok (Sink.try_send_to sink mount seen_events (Seen "store"));
  ignore (Store.drain_events production 8);
  Alcotest.check strings "production" [ "store"; "worker" ] (Store.state production);

  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  from_worker (Node.sink node) mount;
  ok (Sink.try_send_to (Node.sink node) mount seen_events (Seen "store"));
  Alcotest.check strings "lanes" [ "Worker"; "Store" ] (lanes (Node.held node));
  let nodes = [ driven node ] in
  step_some scenario nodes no_world;
  step_some scenario nodes no_world;
  Alcotest.check strings "as production" (Store.state production) (Node.state node)

let default_scenario_preserves_front_insertion_order () =
  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  let reply = Option.get (Sink.reply_for (Node.sink node) mount seen_events ~answers:1) in
  ok (Reply.send_front reply (Seen "first"));
  ok (Reply.send_front reply (Seen "second"));
  let nodes = [ driven node ] in
  step_some scenario nodes no_world;
  step_some scenario nodes no_world;
  Alcotest.check strings "state" [ "second"; "first" ] (Node.state node)

let request node = { Request_id.session = Node.session node; execution = 1; index = 0 }

(* Rust drops the Timer; the OCaml one is stopped, which drops a simulated
   timer's pending timers. *)
let dropping_a_simulated_timer_cancels_its_pending_alarms () =
  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  let reply = Option.get (Sink.reply_for (Node.sink node) mount Timer.fired ~answers:1) in
  let timer, control = Timer.simulated (Scenario.clock scenario) in
  Timer.after timer ~delay:1 reply (request node);
  Timer.stop timer;
  Clock.Manual.advance (Scenario.clock scenario) 1;
  Alcotest.(check int)
    "a dropped Timer must never publish another alarm" 0
    (Timer.Control.fire_due control);
  Alcotest.(check int) "nothing held" 0 (List.length (Node.held node))

let simulated_timer_uses_the_worker_lane_like_the_threaded_timer () =
  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  let reply = Option.get (Sink.reply_for (Node.sink node) mount Timer.fired ~answers:1) in
  let timer, control = Timer.simulated (Scenario.clock scenario) in
  Timer.after timer ~delay:0 reply (request node);
  Alcotest.(check int) "fired" 1 (Timer.Control.fire_due control);
  Alcotest.(check string)
    "lane" "Worker"
    (lane_string (List.hd (Node.held node)).held_lane)

let a_mixed_backlog_runs_in_production_order () =
  let production, sink, mount = production_store 8 in
  ignore (Store.dispatch_exn production mount Prime);
  let front sink mount note =
    ok
      (Reply.send_front
         (Option.get (Sink.reply_for sink mount seen_events ~answers:1))
         (Seen note))
  in
  let backlog sink mount =
    Thread.join
      (Thread.create
         (fun () -> ok (Sink.try_send_to sink mount seen_events (Seen "w1")))
         ());
    ok (Sink.try_send_to sink mount seen_events (Seen "b1"));
    front sink mount "f1";
    ok (Sink.try_send_to sink mount seen_events (Seen "b2"));
    front sink mount "f2";
    Thread.join
      (Thread.create
         (fun () -> ok (Sink.try_send_to sink mount seen_events (Seen "w2")))
         ())
  in
  backlog sink mount;
  ignore (Store.drain_events production 16);
  Alcotest.check strings "production"
    [ "f2"; "f1"; "b1"; "b2"; "w1"; "w2" ]
    (Store.state production);

  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  backlog (Node.sink node) mount;
  let nodes = [ driven node ] in
  while Scenario.step scenario nodes no_world <> None do
    ()
  done;
  Alcotest.check strings "as production" (Store.state production) (Node.state node)

(* A front send from a worker thread is held as one to the head of the
   worker lane, and the default scenario runs it where production would:
   after the Store lane, ahead of the rest of the worker lane. Before, it
   was held as a plain worker send, and ran last. *)
let a_front_send_from_a_worker_runs_in_production_order () =
  let production, sink, mount = production_store 8 in
  ignore (Store.dispatch_exn production mount Prime);
  let front sink mount note =
    ok
      (Reply.send_front
         (Option.get (Sink.reply_for sink mount seen_events ~answers:1))
         (Seen note))
  in
  let backlog sink mount =
    Thread.join
      (Thread.create
         (fun () ->
           ok (Sink.try_send_to sink mount seen_events (Seen "w1"));
           front sink mount "wf1";
           front sink mount "wf2";
           ok (Sink.try_send_to sink mount seen_events (Seen "w2")))
         ());
    ok (Sink.try_send_to sink mount seen_events (Seen "b1"));
    front sink mount "f1"
  in
  backlog sink mount;
  ignore (Store.drain_events production 16);
  Alcotest.check strings "production"
    [ "f1"; "b1"; "wf2"; "wf1"; "w1"; "w2" ]
    (Store.state production);

  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  backlog (Node.sink node) mount;
  Alcotest.check strings "lanes"
    [ "Worker"; "Worker"; "Worker"; "Worker"; "Store"; "Store" ]
    (lanes (Node.held node));
  Alcotest.(check (list bool))
    "fronts"
    [ false; true; true; false; false; true ]
    (List.map (fun e -> e.held_front) (Node.held node));
  Alcotest.(check (option int))
    "the Store lane's front first" (Some 5)
    (next_in_queue_order (Node.held node));
  Alcotest.(check (option int))
    "then the worker lane's newest front" (Some 2)
    (next_in_queue_order
       (List.filter (fun e -> e.held_lane = Worker_lane) (Node.held node)));
  let nodes = [ driven node ] in
  while Scenario.step scenario nodes no_world <> None do
    ()
  done;
  Alcotest.check strings "as production" (Store.state production) (Node.state node)

let a_simulated_worker_keeps_the_worker_lane_on_the_driver_thread () =
  let scenario = Scenario.create 1 1_000 in
  let node, mount = node scenario in
  let sink = Node.sink node in
  (* Stands in for a thread: publishes once the clock reaches [at]. *)
  let at = ref (Some 1_001) in
  let world =
    {
      next_time = (fun () -> !at);
      advance =
        (fun now ->
          match !at with
          | Some t when t <= now ->
              at := None;
              ok
                (Sink.try_send_to_as_worker sink mount seen_events
                   (Seen "external worker"));
              let reply = Option.get (Sink.reply_for sink mount seen_events ~answers:1) in
              ok (Reply.try_send_as_worker reply (Seen "worker reply"))
          | _ -> ());
    }
  in
  let nodes = [ driven node ] in
  step_some scenario nodes world;
  Alcotest.check strings "lanes" [ "Worker"; "Worker" ] (lanes (Node.held node));
  (* The plain sends from this thread, the Store's, take the Store lane. *)
  ok (Sink.try_send_to sink mount seen_events (Seen "store"));
  Alcotest.(check string)
    "lane" "Store"
    (lane_string (List.nth (Node.held node) 2).held_lane)

let worker_sends_from_the_store_thread_are_refused_when_the_lane_is_full () =
  let store, sink, mount = production_store 1 in
  ignore (Store.dispatch_exn store mount Prime);
  ok (Sink.try_send_to_as_worker sink mount seen_events (Seen "w1"));
  Alcotest.(check bool)
    "full" true
    (match Sink.try_send_to_as_worker sink mount seen_events (Seen "w2") with
    | Error (Full _) -> true
    | _ -> false);
  ok (Sink.try_send_to sink mount seen_events (Seen "store"));
  (* the Store lane is not bounded *)
  ignore (Store.drain_events store 8);
  Alcotest.check strings "state" [ "store"; "w1" ] (Store.state store)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "queue_order"
    [
      ( "queue_order",
        [
          t "default scenario preserves store lane priority"
            default_scenario_preserves_store_lane_priority;
          t "default scenario preserves front insertion order"
            default_scenario_preserves_front_insertion_order;
          t "dropping a simulated timer cancels its pending alarms"
            dropping_a_simulated_timer_cancels_its_pending_alarms;
          t "simulated timer uses the worker lane like the threaded timer"
            simulated_timer_uses_the_worker_lane_like_the_threaded_timer;
          t "a mixed backlog runs in production order"
            a_mixed_backlog_runs_in_production_order;
          t "a front send from a worker runs in production order"
            a_front_send_from_a_worker_runs_in_production_order;
          t "a simulated worker keeps the worker lane on the driver thread"
            a_simulated_worker_keeps_the_worker_lane_on_the_driver_thread;
          t "worker sends from the store thread are refused when the lane is full"
            worker_sends_from_the_store_thread_are_refused_when_the_lane_is_full;
        ] );
    ]
