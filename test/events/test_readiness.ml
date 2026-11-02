open Causet
open Downloads

let pushed note = Pushed { note }
let notify count () = ignore (Atomic.fetch_and_add count 1)

let count label expected counter =
  Alcotest.(check int) label expected (Atomic.get counter)

let subscribe ready wake =
  match Readiness.subscribe ready wake with
  | Ok sub -> sub
  | Error Already_subscribed -> Alcotest.fail "already subscribed"

let blocking ready =
  match Readiness.Blocking.create ready with
  | Ok waiter -> waiter
  | Error Already_subscribed -> Alcotest.fail "already subscribed"

let ready label expected handle =
  let actual =
    Readiness.current handle
    |> Option.map (function Readiness.Events -> "events" | Closed -> "closed")
  in
  Alcotest.(check (option string)) label expected actual

let send f note =
  Alcotest.check download_send_result "send" (Ok ())
    (Sink.try_send_to f.sink f.a download_events (pushed note))

let persistent_and_coalesced () =
  let f = fixture inline 4 in
  let handle = Store.readiness f.store in
  let wakes = Atomic.make 0 in
  let sub = subscribe handle (notify wakes) in
  ready "empty" None handle;
  send f "first";
  send f "second";
  count "one wake for a burst" 1 wakes;
  Alcotest.(check (list string)) "not reduced" [] (Store.state f.store).a.pushed;
  ignore (Store.drain_events f.store 1);
  send f "still queued";
  count "still ready" 1 wakes;
  ignore (Store.drain_events f.store 4);
  send f "new batch";
  count "persistent registration" 2 wakes;
  Store.close f.store;
  count "closure" 3 wakes;
  Store.close f.store;
  count "closure once" 3 wakes;
  Readiness.cancel sub

let duplicate_and_cancel () =
  let f = fixture inline 2 in
  let handle = Store.readiness f.store in
  let first = subscribe handle ignore in
  (match Readiness.subscribe handle ignore with
  | Error Already_subscribed -> ()
  | Ok _ -> Alcotest.fail "duplicate accepted");
  Readiness.cancel first;
  let wakes = Atomic.make 0 in
  let next = subscribe handle (notify wakes) in
  Readiness.cancel first;
  send f "kept";
  count "old cancellation leaves replacement" 1 wakes;
  Readiness.cancel next;
  Readiness.cancel next;
  let immediate = subscribe handle (notify wakes) in
  count "already queued" 2 wakes;
  Readiness.cancel immediate;
  Store.close f.store

let capture_release () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  let weak = Weak.create 1 in
  let sub =
    let captured = ref 42 in
    Weak.set weak 0 (Some captured);
    subscribe handle (fun () -> ignore (Sys.opaque_identity captured))
  in
  Gc.full_major ();
  Alcotest.(check bool) "callback retained" true (Weak.check weak 0);
  Readiness.cancel sub;
  Gc.full_major ();
  Alcotest.(check bool) "cancel releases captures" false (Weak.check weak 0);
  ignore (Sys.opaque_identity sub);
  let closed_sub =
    let captured = ref 43 in
    Weak.set weak 0 (Some captured);
    subscribe handle (fun () -> ignore (Sys.opaque_identity captured))
  in
  Store.close f.store;
  Gc.full_major ();
  Alcotest.(check bool)
    "closure releases captures with token alive" false (Weak.check weak 0);
  ignore (Sys.opaque_identity closed_sub)

let held_events () =
  let p = protocol () in
  let sink, inbox, held = Event_bus.held p ~worker_capacity:1 1 in
  let handle = Inbox.readiness inbox in
  let s = Store.create_exn p ~state:empty_app ~services:{ downloads = inline } inbox in
  let a = Protocol.resolve_exn p (Path.child (Path.root app) a_slot) in
  let wakes = Atomic.make 0 in
  let sub = subscribe handle (notify wakes) in
  ignore (Sink.try_send_to_as_worker sink a download_events (pushed "lost"));
  let id = (List.hd (Held.list held)).held_id in
  ignore (Held.discard held id);
  ready "held is not ready" None handle;
  count "discard silent" 0 wakes;
  ignore (Sink.try_send_to_as_worker sink a download_events (pushed "released"));
  let id = (List.hd (Held.list held)).held_id in
  (match Held.release held id with Ok () -> () | Error _ -> Alcotest.fail "release");
  count "release notifies" 1 wakes;
  ignore (Store.drain_events s 1);
  Store.close s;
  count "closure notifies" 2 wakes;
  Readiness.cancel sub

let closure_and_fault () =
  List.iter
    (fun fault ->
      let f = fixture inline 1 in
      let handle = Store.readiness f.store in
      let wakes = Atomic.make 0 in
      let sub = subscribe handle (notify wakes) in
      if fault then ignore (Store.dispatch_to f.store f.a Crash) else Store.close f.store;
      ready "closed" (Some "closed") handle;
      count "notified" 1 wakes;
      let next = subscribe handle (notify wakes) in
      count "closed subscribe is immediate" 2 wakes;
      Readiness.cancel sub;
      Readiness.cancel next;
      Store.close f.store)
    [ false; true ];
  let _, inbox = Event_bus.create (protocol ()) ~worker_capacity:1 in
  let handle = Inbox.readiness inbox in
  let wakes = Atomic.make 0 in
  let sub = subscribe handle (notify wakes) in
  ignore
    (Store.create (protocol ()) ~state:empty_app ~services:{ downloads = inline } inbox);
  ready "refused inbox closes" (Some "closed") handle;
  count "refusal notifies" 1 wakes;
  Readiness.cancel sub

let reentry_and_notification_failure () =
  let f = fixture inline 0 in
  let handle = Store.readiness f.store in
  let wakes = Atomic.make 0 in
  let sub =
    subscribe handle (fun () ->
        ready "callback can reenter" (Some "events") handle;
        notify wakes ();
        failwith "bad host notifier")
  in
  (match Store.dispatch_to f.store f.a (Fetch "ready") with
  | Ok Executed -> ()
  | _ -> Alcotest.fail "callback faulted the Store");
  count "store lane notifies" 1 wakes;
  Alcotest.check drained "input retained" (Ok 1) (Store.drain_events f.store 1);
  Readiness.cancel sub;
  Store.close f.store

let registration_race () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  for _ = 1 to 128 do
    let start = Atomic.make false in
    let wakes = Atomic.make 0 in
    let sender =
      Thread.create
        (fun () ->
          while not (Atomic.get start) do
            Thread.yield ()
          done;
          ignore (Sink.try_send_to_as_worker f.sink f.a download_events (pushed "raced")))
        ()
    in
    Atomic.set start true;
    let sub = subscribe handle (notify wakes) in
    Thread.join sender;
    count "arrival is not lost" 1 wakes;
    Alcotest.check drained "drain" (Ok 1) (Store.drain_events f.store 1);
    Readiness.cancel sub
  done;
  Store.close f.store

let blocking_waits () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  let waiter = blocking handle in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close f.store)
    (fun () ->
      Alcotest.(check bool)
        "empty zero timeout" true
        (Readiness.Blocking.wait_timeout waiter ~timeout:0. = None);
      for _ = 1 to 16 do
        let worker =
          Thread.create
            (fun () ->
              Thread.delay 0.005;
              ignore
                (Sink.try_send_to_as_worker f.sink f.a download_events (pushed "woke")))
            ()
        in
        let started = Unix.gettimeofday () in
        Alcotest.(check bool)
          "event wakes wait" true
          (Readiness.Blocking.wait_timeout waiter ~timeout:5. = Some Readiness.Events);
        Alcotest.(check bool) "before timeout" true (Unix.gettimeofday () -. started < 2.);
        Thread.join worker;
        Alcotest.check drained "wait did not consume" (Ok 1)
          (Store.drain_events f.store 1)
      done;
      Alcotest.(check bool)
        "empty timeout after a stale wake byte" true
        (Readiness.Blocking.wait_timeout waiter ~timeout:0.002 = None);
      let closer =
        Thread.create
          (fun () ->
            Thread.delay 0.005;
            Store.close f.store)
          ()
      in
      let started = Unix.gettimeofday () in
      Alcotest.(check bool)
        "closure wakes wait" true
        (Readiness.Blocking.wait_timeout waiter ~timeout:5. = Some Readiness.Closed);
      Alcotest.(check bool) "before timeout" true (Unix.gettimeofday () -. started < 2.);
      Thread.join closer)

let blocking_lifecycle () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  let next_fd () =
    let fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
    Unix.close fd;
    fd
  in
  let before = if Sys.win32 then None else Some (next_fd ()) in
  for _ = 1 to 32 do
    let waiter = blocking handle in
    (match Readiness.Blocking.create handle with
    | Error Already_subscribed -> ()
    | Ok _ -> Alcotest.fail "duplicate waiter");
    Readiness.Blocking.close waiter;
    Readiness.Blocking.close waiter
  done;
  Option.iter
    (fun before ->
      Alcotest.(check bool)
        "all socket ends closed, including refused waiters" true
        (before = next_fd ()))
    before;
  let sub = subscribe handle ignore in
  Readiness.cancel sub;
  Store.close f.store

let wait waiter timeout = Readiness.Blocking.wait_timeout waiter ~timeout

let status label expected actual =
  let name = Option.map (function Readiness.Events -> "events" | Closed -> "closed") in
  Alcotest.(check (option string)) label (name expected) (name actual)

(* Waits on [waiter] while [f] runs on another thread after 5 ms, and
   returns what the wait returned and what [f] did. *)
let wait_while waiter f =
  let other =
    on_thread (fun () ->
        Thread.delay 0.005;
        f ())
  in
  let started = Unix.gettimeofday () in
  let woke = wait waiter 5. in
  Alcotest.(check bool) "before the timeout" true (Unix.gettimeofday () -. started < 2.);
  (woke, join other)

let a_held_events_release_wakes_a_blocking_wait () =
  let p = protocol () in
  let sink, inbox, held = Event_bus.held p ~worker_capacity:1 1 in
  let s = Store.create_exn p ~state:empty_app ~services:{ downloads = inline } inbox in
  let a = Protocol.resolve_exn p (Path.child (Path.root app) a_slot) in
  let waiter = blocking (Inbox.readiness inbox) in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close s)
    (fun () ->
      Alcotest.check download_send_result "held" (Ok ())
        (Sink.try_send_to_as_worker sink a download_events (pushed "held"));
      status "a held Event is not ready" None (wait waiter 0.002);
      let id = (List.hd (Held.list held)).held_id in
      let woke, released = wait_while waiter (fun () -> Held.release held id) in
      status "the release wakes the wait" (Some Readiness.Events) woke;
      Alcotest.(check bool) "released" true (Result.is_ok released);
      Alcotest.check drained "the wait did not consume it" (Ok 1) (Store.drain_events s 1))

let a_fault_wakes_a_blocking_wait_with_closed () =
  let f = fixture inline 1 in
  let waiter = blocking (Store.readiness f.store) in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close f.store)
    (fun () ->
      let woke, crashed =
        wait_while waiter (fun () -> Store.dispatch_to f.store f.a Crash)
      in
      status "the fault wakes the wait" (Some Readiness.Closed) woke;
      (match crashed with
      | Error (Faulted _) -> ()
      | _ -> Alcotest.fail "the reducer did not fault the Store");
      status "and it stays closed" (Some Readiness.Closed) (wait waiter 5.))

let a_waiter_made_on_a_closed_bus_returns_closed_at_once () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  Store.close f.store;
  let first = blocking handle in
  (* A closed bus keeps no registration, so it refuses no second waiter. *)
  let second = blocking handle in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close first;
      Readiness.Blocking.close second)
    (fun () ->
      status "at once" (Some Readiness.Closed) (wait first 0.);
      let started = Unix.gettimeofday () in
      status "without waiting" (Some Readiness.Closed) (wait first 5.);
      Alcotest.(check bool)
        "before the timeout" true
        (Unix.gettimeofday () -. started < 2.);
      status "the second waiter too" (Some Readiness.Closed) (wait second 0.))

let a_waiter_and_a_subscription_refuse_each_other () =
  let f = fixture inline 1 in
  let handle = Store.readiness f.store in
  let sub = subscribe handle ignore in
  (match Readiness.Blocking.create handle with
  | Error Already_subscribed -> ()
  | Ok waiter ->
      Readiness.Blocking.close waiter;
      Alcotest.fail "a waiter beside a subscription");
  Readiness.cancel sub;
  let waiter = blocking handle in
  (match Readiness.subscribe handle ignore with
  | Error Already_subscribed -> ()
  | Ok sub ->
      Readiness.cancel sub;
      Alcotest.fail "a subscription beside a waiter");
  Readiness.Blocking.close waiter;
  let wakes = Atomic.make 0 in
  let sub = subscribe handle (notify wakes) in
  send f "after the waiter";
  count "the subscription notifies" 1 wakes;
  Readiness.cancel sub;
  Store.close f.store

let waiting_on_a_closed_waiter_or_with_an_invalid_timeout_raises () =
  let f = fixture inline 1 in
  let waiter = blocking (Store.readiness f.store) in
  let raises label timeout =
    match wait waiter timeout with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.failf "%s: no Invalid_argument" label
  in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close f.store)
    (fun () ->
      (* Accepted, a negative timeout returns at once, while NaN and infinity
         wait on: it comes first, so a missing check fails rather than hangs. *)
      raises "negative" (-0.001);
      raises "NaN" Float.nan;
      raises "infinite" Float.infinity;
      raises "negative infinite" Float.neg_infinity;
      status "the waiter still works" None (wait waiter 0.);
      Readiness.Blocking.close waiter;
      raises "a closed waiter" 0.;
      send f "queued";
      raises "a closed waiter on a ready bus" 0.;
      Store.close f.store;
      raises "a closed waiter on a closed bus" 0.)

(* The thread that runs the Store moves between calls: its sends then take
   the Store lane, and the old thread's take the worker lane. The worker lane
   holds nothing here, so a reply that took it would be refused, and the
   inline Service would fault the Store. *)
let a_store_handed_to_another_thread_keeps_its_store_lane () =
  let f = fixture inline 0 in
  let waiter = blocking (Store.readiness f.store) in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close f.store)
    (fun () ->
      ok_dispatch f.store f.a (Fetch "here");
      status "ready on the first thread" (Some Readiness.Events) (wait waiter 5.);
      Alcotest.check drained "drained on the first thread" (Ok 1)
        (Store.drain_events f.store 1);
      let woke, drained_there =
        in_thread (fun () ->
            ok_dispatch f.store f.a (Fetch "there");
            let woke = wait waiter 5. in
            (woke, Store.drain_events f.store 1))
      in
      status "ready on the new thread" (Some Readiness.Events) woke;
      Alcotest.check drained "drained on the new thread" (Ok 1) drained_there;
      Alcotest.check download_send_result "the old thread takes the worker lane"
        (Error (Full (pushed "old thread")))
        (Sink.try_send_to f.sink f.a download_events (pushed "old thread"));
      status "the refused send left nothing" None (wait waiter 0.);
      ok_dispatch f.store f.a (Fetch "back");
      status "ready once handed back" (Some Readiness.Events) (wait waiter 5.);
      Alcotest.check drained "drained once handed back" (Ok 1)
        (Store.drain_events f.store 1);
      Alcotest.(check (list (pair string int)))
        "all three replies reduced"
        [ ("here", 4); ("there", 5); ("back", 4) ]
        (Store.state f.store).a.done_)

(* A send writes a wake byte that no wait reads before the drain empties the
   bus. The next wait takes the byte and sleeps; left in the socket, it would
   wake the wait at once, again and again, until the timeout. *)
let a_leftover_wake_byte_does_not_spin_the_wait () =
  let f = fixture inline 1 in
  let waiter = blocking (Store.readiness f.store) in
  Fun.protect
    ~finally:(fun () ->
      Readiness.Blocking.close waiter;
      Store.close f.store)
    (fun () ->
      send f "leftover";
      Alcotest.check drained "drained without waiting" (Ok 1)
        (Store.drain_events f.store 1);
      let cpu () =
        let t = Unix.times () in
        t.tms_utime +. t.tms_stime
      in
      let cpu_before = cpu () in
      let started = Unix.gettimeofday () in
      status "the wait times out" None (wait waiter 0.5);
      let took = Unix.gettimeofday () -. started in
      let used = cpu () -. cpu_before in
      Alcotest.(check bool) "it waited out the timeout" true (took >= 0.45);
      if used >= 0.25 then Alcotest.failf "the wait used %.3fs of CPU in %.3fs" used took)

(* Keep the wake sockets above Unix select's descriptor-number limit.
   Windows handles have a different representation and no such limit; its
   socket backend is covered by the ordinary readiness and timer tests. *)
let waits_and_threaded_timers_work_past_1024_descriptors () =
  if Sys.win32 then Alcotest.skip ();
  let number (fd : Unix.file_descr) : int = Obj.magic fd in
  let opened = ref [] in
  let keep fd = opened := fd :: !opened in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close !opened)
    (fun () ->
      (match
         keep (Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0);
         let source = List.hd !opened in
         while number (List.hd !opened) < 1024 do
           keep (Unix.dup ~cloexec:true source)
         done
       with
      | () -> ()
      | exception Unix.Unix_error ((Unix.EMFILE | Unix.ENFILE), _, _) ->
          let highest = match !opened with fd :: _ -> number fd | [] -> -1 in
          Printf.printf
            "Skipped: no descriptor past %d could be opened, under 1024. Run it under \
             ulimit -n 4096.\n\
             %!"
            highest;
          Alcotest.skip ());
      let f = fixture inline 1 in
      let waiter = blocking (Store.readiness f.store) in
      Fun.protect
        ~finally:(fun () ->
          Readiness.Blocking.close waiter;
          Store.close f.store)
        (fun () ->
          status "a high descriptor times out" None (wait waiter 0.002);
          let woke, sent =
            wait_while waiter (fun () ->
                Sink.try_send_to_as_worker f.sink f.a download_events (pushed "high"))
          in
          status "a high descriptor wakes" (Some Readiness.Events) woke;
          Alcotest.check download_send_result "sent" (Ok ()) sent;
          Alcotest.check drained "the wait did not consume it" (Ok 1)
            (Store.drain_events f.store 1);
          status "a stale wake byte is cleared" None (wait waiter 0.002);
          let timer = Timer.create () in
          let ap = Alarms.protocol () in
          let _, inbox = Event_bus.create ap ~worker_capacity:16 in
          let alarms = Store.create_exn ap ~state:Alarms.empty ~services:timer inbox in
          let timer_waiter = blocking (Store.readiness alarms) in
          Fun.protect
            ~finally:(fun () ->
              Readiness.Blocking.close timer_waiter;
              Store.close alarms;
              Timer.stop timer)
            (fun () ->
              (* Let the timer enter its indefinite idle wait first. *)
              Thread.delay 0.005;
              Alarms.set alarms (Alarms.mount ap) "soon" 10;
              status "a threaded timer wakes the driver" (Some Readiness.Events)
                (wait timer_waiter 5.);
              ignore (Alarms.drain alarms 16);
              Alcotest.(check (list string))
                "the threaded timer fired" [ "soon" ] (Store.state alarms).Alarms.fired;
              Alarms.set alarms (Alarms.mount ap) "later" 60_000)))

let () =
  Alcotest.run "readiness"
    [
      ( "readiness",
        [
          Alcotest.test_case "persistent and coalesced" `Quick persistent_and_coalesced;
          Alcotest.test_case "one registration and cancellation" `Quick
            duplicate_and_cancel;
          Alcotest.test_case "capture release" `Quick capture_release;
          Alcotest.test_case "held release" `Quick held_events;
          Alcotest.test_case "closure and faults" `Quick closure_and_fault;
          Alcotest.test_case "reentry and callback failure" `Quick
            reentry_and_notification_failure;
          Alcotest.test_case "registration races" `Quick registration_race;
          Alcotest.test_case "blocking wakeups and timeouts" `Quick blocking_waits;
          Alcotest.test_case "blocking lifecycle" `Quick blocking_lifecycle;
          Alcotest.test_case "a held Event's release wakes a blocking wait" `Quick
            a_held_events_release_wakes_a_blocking_wait;
          Alcotest.test_case "a fault wakes a blocking wait with Closed" `Quick
            a_fault_wakes_a_blocking_wait_with_closed;
          Alcotest.test_case "a waiter made on a closed bus returns Closed at once" `Quick
            a_waiter_made_on_a_closed_bus_returns_closed_at_once;
          Alcotest.test_case "a waiter and a subscription refuse each other" `Quick
            a_waiter_and_a_subscription_refuse_each_other;
          Alcotest.test_case
            "waiting on a closed waiter or with an invalid timeout raises" `Quick
            waiting_on_a_closed_waiter_or_with_an_invalid_timeout_raises;
          Alcotest.test_case "a Store handed to another thread keeps its Store lane"
            `Quick a_store_handed_to_another_thread_keeps_its_store_lane;
          Alcotest.test_case "a leftover wake byte does not spin the wait" `Quick
            a_leftover_wake_byte_does_not_spin_the_wait;
          Alcotest.test_case "waits and threaded timers work past 1024 descriptors" `Quick
            waits_and_threaded_timers_work_past_1024_descriptors;
        ] );
    ]
