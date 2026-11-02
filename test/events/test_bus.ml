(* The Event bus's worker lane: bounded, waiting senders woken by the drain
   or by the Store closing (the Rust port's tests/bus.rs). *)

open Causet
open Downloads

let pushed note = Pushed { note }

let a_waiting_sender_wakes_when_the_store_drains () =
  let f = fixture inline 1 in
  let sink, a = (f.sink, f.a) in
  Alcotest.check download_send_result "first, from a worker" (Ok ())
    (in_thread (fun () -> Sink.try_send_to sink a download_events (pushed "first")));

  (* The lane is full; this sender waits instead of failing. *)
  let waiting =
    on_thread (fun () ->
        let started = Clock.monotonic_seconds () in
        let result =
          Sink.send_timeout_to sink a download_events (pushed "second") ~timeout:10.0
        in
        (result, Clock.monotonic_seconds () -. started))
  in
  Thread.delay 0.05;
  Alcotest.check drained "one drained" (Ok 1) (Store.drain_events f.store 1);

  let result, waited = join waiting in
  Alcotest.check download_send_result "second sent" (Ok ()) result;
  if waited >= 5.0 then
    Alcotest.failf "woken by the drain, not by the timeout (%.3fs)" waited;
  Alcotest.check drained "the second drained" (Ok 1) (Store.drain_events f.store 16);
  Alcotest.(check (list string))
    "pushed" [ "first"; "second" ] (Store.state f.store).a.pushed;
  Store.close f.store

let send_timeout_gives_up_after_its_bound () =
  let f = fixture inline 1 in
  let sink, a = (f.sink, f.a) in
  let first, result, waited =
    in_thread (fun () ->
        let first = Sink.try_send_to sink a download_events (pushed "first") in
        let started = Clock.monotonic_seconds () in
        let result =
          Sink.send_timeout_to sink a download_events (pushed "second") ~timeout:0.05
        in
        (first, result, Clock.monotonic_seconds () -. started))
  in
  Alcotest.check download_send_result "first" (Ok ()) first;
  Alcotest.check download_send_result "second refused"
    (Error (Full (pushed "second")))
    result;
  if waited < 0.05 then Alcotest.failf "gave up before its bound (%.4fs)" waited;
  Store.close f.store

let closing_the_store_wakes_a_waiting_sender_with_closed () =
  let f = fixture inline 1 in
  let sink, a = (f.sink, f.a) in
  Alcotest.check download_send_result "first" (Ok ())
    (Sink.try_send_to sink a download_events (pushed "first"));
  let waiting =
    on_thread (fun () ->
        Sink.send_timeout_to sink a download_events (pushed "second") ~timeout:10.0)
  in
  Thread.delay 0.05;
  (* Rust: drop(f). *)
  Store.close f.store;
  Alcotest.check download_send_result "woken with Closed"
    (Error (Closed (pushed "second")))
    (join waiting)

(* One Store takes an inbox: a second is refused, and the first keeps its
   bus, its Events and its session. Before, both shared the bus: an Event
   sent for the first was drained by the second, and closing either closed
   the other's. *)
let an_inbox_belongs_to_one_store () =
  let proto = protocol () in
  let sink, inbox = Event_bus.create proto ~worker_capacity:4 in
  let services = { downloads = inline } in
  let first = Store.create_exn proto ~state:empty_app ~services inbox in
  (match Store.create proto ~state:empty_app ~services inbox with
  | exception Invalid_argument m ->
      Alcotest.(check string) "refused" "causet: this inbox already belongs to a Store" m
  | _ -> Alcotest.fail "a second Store took the inbox");
  let a = Protocol.resolve_exn proto (Path.child (Path.root app) a_slot) in
  Alcotest.check download_send_result "sent" (Ok ())
    (Sink.try_send_to sink a download_events (pushed "mine"));
  Alcotest.check drained "drained by the first" (Ok 1) (Store.drain_events first 16);
  Alcotest.(check (list string)) "pushed" [ "mine" ] (Store.state first).a.pushed;
  Store.close first

(* A faulted Store closes its bus at once, as [Store.close] does: every
   later send fails with [Closed], from any thread and through any route,
   and a sender waiting for room is woken with it. Before, the bus stayed
   open after a fault, and Services' sends kept filling the worker lane
   until the Store was closed. *)
let a_faulted_store_closes_its_bus () =
  let f = fixture inline 1 in
  let sink, a = (f.sink, f.a) in
  (* A root input tells the bus which thread runs the Store. *)
  ok_dispatch f.store f.counter Inc;
  Alcotest.check download_send_result "queued on the Store lane" (Ok ())
    (Sink.try_send_to sink a download_events (pushed "store"));
  Alcotest.check download_send_result "queued on the worker lane" (Ok ())
    (in_thread (fun () -> Sink.try_send_to sink a download_events (pushed "worker")));
  (* The worker lane is full: this sender waits for room. *)
  let waiting =
    on_thread (fun () ->
        Sink.send_timeout_to sink a download_events (pushed "waiting") ~timeout:10.0)
  in
  Thread.delay 0.05;
  Alcotest.check dispatched "the crash faults the Store"
    (Error (Faulted "downloader crashed"))
    (Store.dispatch_to f.store a Crash);
  Alcotest.check download_send_result "the waiting sender is woken with Closed"
    (Error (Closed (pushed "waiting")))
    (join waiting);
  let reply = Option.get (Sink.reply_for sink a download_events ~answers:1) in
  let closed name result =
    Alcotest.check download_send_result name (Error (Closed (pushed name))) result
  in
  closed "sink" (Sink.try_send_to sink a download_events (pushed "sink"));
  closed "sink as worker"
    (Sink.try_send_to_as_worker sink a download_events (pushed "sink as worker"));
  closed "sink with a timeout"
    (Sink.send_timeout_to sink a download_events
       (pushed "sink with a timeout")
       ~timeout:1.0);
  closed "reply" (Reply.try_send reply (pushed "reply"));
  closed "reply as worker" (Reply.try_send_as_worker reply (pushed "reply as worker"));
  closed "reply to the front" (Reply.send_front reply (pushed "reply to the front"));
  closed "reply with a timeout"
    (Reply.send_timeout reply (pushed "reply with a timeout") ~timeout:1.0);
  closed "from a worker"
    (in_thread (fun () -> Reply.try_send reply (pushed "from a worker")));
  closed "to the front from a worker"
    (in_thread (fun () -> Reply.send_front reply (pushed "to the front from a worker")));
  Alcotest.check drained "the Store drains nothing" (Error (Faulted "downloader crashed"))
    (Store.drain_events f.store 16);
  Alcotest.(check (list string)) "nothing queued ran" [] (Store.state f.store).a.pushed

(* On a held bus, a fault drops what is held, as closing does. *)
let a_faulted_store_drops_what_its_held_bus_holds () =
  let proto = protocol () in
  let sink, inbox, held = Event_bus.held proto ~worker_capacity:4 1 in
  let store =
    Store.create_exn proto ~state:empty_app ~services:{ downloads = inline } inbox
  in
  let a = Protocol.resolve_exn proto (Path.child (Path.root app) a_slot) in
  ok_dispatch store a (Note 1);
  Alcotest.check download_send_result "held" (Ok ())
    (Sink.try_send_to sink a download_events (pushed "store"));
  Alcotest.check download_send_result "held from a worker" (Ok ())
    (in_thread (fun () -> Sink.try_send_to sink a download_events (pushed "worker")));
  let first = List.hd (Held.list held) in
  Alcotest.(check int) "two held" 2 (Held.length held);
  Alcotest.check dispatched "the crash faults the Store"
    (Error (Faulted "downloader crashed"))
    (Store.dispatch_to store a Crash);
  Alcotest.(check bool) "nothing held" true (Held.is_empty held);
  Alcotest.(check bool)
    "a release finds the bus closed" true
    (Held.release held first.held_id = Error Release_closed);
  Alcotest.check download_send_result "later sends are refused"
    (Error (Closed (pushed "later")))
    (Sink.try_send_to sink a download_events (pushed "later"))

(* A front send goes to the head of the lane it takes: the Store lane's
   from the Store's thread, the worker lane's from any other, where the
   lane's capacity bounds it and a full lane refuses it at once. Before, a
   front send from another thread went to the back of the worker lane. *)
let send_front_takes_the_head_of_its_lane () =
  let f = fixture inline 2 in
  let sink, a = (f.sink, f.a) in
  ok_dispatch f.store f.counter Inc;
  let reply = Option.get (Sink.reply_for sink a download_events ~answers:1) in
  let sent name result = Alcotest.check download_send_result name (Ok ()) result in
  in_thread (fun () ->
      sent "worker" (Sink.try_send_to sink a download_events (pushed "worker"));
      sent "worker front" (Reply.send_front reply (pushed "worker front"));
      (* The worker lane is full, its head included. *)
      Alcotest.check download_send_result "a full worker lane refuses a front send"
        (Error (Full (pushed "refused")))
        (Reply.send_front reply (pushed "refused")));
  sent "store" (Sink.try_send_to sink a download_events (pushed "store"));
  sent "store front" (Reply.send_front reply (pushed "store front"));
  Alcotest.check drained "one drained" (Ok 1) (Store.drain_events f.store 1);
  Alcotest.check drained "the rest drained" (Ok 3) (Store.drain_events f.store 16);
  Alcotest.(check (list string))
    "the Store lane, then the worker lane, each from its head"
    [ "store front"; "store"; "worker front"; "worker" ]
    (Store.state f.store).a.pushed;
  (* Draining made room: a front send from a worker fits again. *)
  sent "room again" (in_thread (fun () -> Reply.send_front reply (pushed "room again")));
  Store.close f.store

let () =
  Alcotest.run "events.bus"
    [
      ( "bus",
        [
          Alcotest.test_case "a faulted Store closes its bus" `Quick
            a_faulted_store_closes_its_bus;
          Alcotest.test_case "a faulted Store drops what its held bus holds" `Quick
            a_faulted_store_drops_what_its_held_bus_holds;
          Alcotest.test_case "send_front takes the head of its lane" `Quick
            send_front_takes_the_head_of_its_lane;
          Alcotest.test_case "an inbox belongs to one Store" `Quick
            an_inbox_belongs_to_one_store;
          Alcotest.test_case "a waiting sender wakes when the Store drains" `Quick
            a_waiting_sender_wakes_when_the_store_drains;
          Alcotest.test_case "send_timeout gives up after its bound" `Quick
            send_timeout_gives_up_after_its_bound;
          Alcotest.test_case "closing the Store wakes a waiting sender with Closed" `Quick
            closing_the_store_wakes_a_waiting_sender_with_closed;
        ] );
    ]
