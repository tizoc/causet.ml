(* The timer Service: [fired] through the bus, in deadline order, first in
   first out at equal deadlines, and nothing left running once the Store
   closes and its timer stops (the Rust port's tests/timer.rs). *)

open Causet
open Alarms

let store () =
  let p = protocol () in
  let _sink, inbox = Event_bus.create p ~worker_capacity:16 in
  let timer = Timer.create () in
  let store = Store.create_exn p ~state:empty ~services:timer inbox in
  (store, mount p, timer)

let timers_fire_in_deadline_order_and_equal_deadlines_in_the_order_set () =
  let s, alarms, timer = store () in
  Fun.protect
    ~finally:(fun () ->
      Store.close s;
      Timer.stop timer)
    (fun () ->
      (* Deadlines are taken when each timer is set: "late" stays last only if
         the next two are set within 230 ms. *)
      set s alarms "late" 250;
      set s alarms "first" 20;
      set s alarms "second" 20;

      let deadline = Unix.gettimeofday () +. 5.0 in
      while List.length (Store.state s).fired < 3 do
        if Unix.gettimeofday () >= deadline then
          Alcotest.failf "timers did not fire: [%s]"
            (String.concat "; " (Store.state s).fired);
        if drain s 16 = 0 then Thread.delay 0.001
      done;
      Alcotest.(check (list string))
        "fired" [ "first"; "second"; "late" ] (Store.state s).fired)

let closing_the_store_stops_the_timer_with_alarms_still_pending () =
  let s, alarms, timer = store () in
  set s alarms "never" 60_000;
  let started = Unix.gettimeofday () in
  (* Rust: drop(s), which drops its Services; here the Store closes and its
     timer is stopped. *)
  Store.close s;
  Timer.stop timer;
  let took = Unix.gettimeofday () -. started in
  if took >= 1.0 then Alcotest.failf "the timer thread held up the stop (%.3fs)" took

let a_simulated_timer_fires_only_when_its_control_fires_it_on_the_shared_clock () =
  let clock = Clock.Manual.create (1_000 * ms) in
  let timer, control = Timer.simulated clock in
  let p = protocol () in
  let _sink, inbox = Event_bus.create p ~worker_capacity:16 in
  let s =
    Store.create_exn ~clock:(Clock.Manual.clock clock) p ~state:empty ~services:timer
      inbox
  in
  let alarms = mount p in
  set s alarms "late" 250;
  set s alarms "first" 20;
  set s alarms "second" 20;
  Alcotest.(check (pair int (option int)))
    "pending, next deadline"
    (3, Some (1_020 * ms))
    (Timer.Control.pending control, Timer.Control.next_deadline control);

  (* No time passes on its own. *)
  Alcotest.(check int) "nothing due" 0 (Timer.Control.fire_due control);
  Clock.Manual.advance clock (20 * ms);
  Alcotest.(check int) "two due" 2 (Timer.Control.fire_due control);
  Alcotest.(check int) "two drained" 2 (drain s 16);
  Alcotest.(check (list string)) "fired" [ "first"; "second" ] (Store.state s).fired;
  Alcotest.(check (option int))
    "next deadline"
    (Some (1_250 * ms))
    (Timer.Control.next_deadline control);

  Clock.Manual.advance clock (230 * ms);
  Alcotest.(check int) "one due" 1 (Timer.Control.fire_due control);
  Alcotest.(check int) "one drained" 1 (drain s 16);
  Alcotest.(check (list string))
    "fired" [ "first"; "second"; "late" ] (Store.state s).fired;
  Alcotest.(check (pair int (option int)))
    "none pending" (0, None)
    (Timer.Control.pending control, Timer.Control.next_deadline control)

let a_host_drives_an_arbitrary_clock_and_retries_a_full_lane () =
  let now = ref (1_000 * ms) in
  let clock : Clock.t = { now = (fun () -> !now) } in
  let timer, control = Timer.driven clock in
  let p = protocol () in
  let _, inbox = Event_bus.create p ~worker_capacity:1 in
  let s = Store.create_exn ~clock p ~state:empty ~services:timer inbox in
  let alarms = mount p in
  Fun.protect
    ~finally:(fun () ->
      Store.close s;
      Timer.stop timer)
    (fun () ->
      set s alarms "late" 250;
      set s alarms "first" 20;
      set s alarms "second" 20;
      Alcotest.(check (option int))
        "host deadline"
        (Some (1_020 * ms))
        (Timer.Control.next_deadline control);
      now := 1_250 * ms;
      Alcotest.(check int) "clock movement alone sends nothing" 0 (drain s 1);
      Alcotest.(check int) "first fills the lane" 1 (Timer.Control.fire_due control);
      Alcotest.(check (option int))
        "second stays due"
        (Some (1_020 * ms))
        (Timer.Control.next_deadline control);
      Alcotest.(check int) "later timers retained" 2 (Timer.Control.pending control);
      Alcotest.(check bool)
        "event readiness sees the timer" true
        (Readiness.current (Store.readiness s) = Some Readiness.Events);
      Alcotest.(check (list string)) "not reduced yet" [] (Store.state s).fired;
      Alcotest.(check int) "first drained" 1 (drain s 1);
      Alcotest.(check int) "second retries" 1 (Timer.Control.fire_due control);
      Alcotest.(check int) "second drained" 1 (drain s 1);
      Alcotest.(check int) "late retries" 1 (Timer.Control.fire_due control);
      Alcotest.(check int) "late drained" 1 (drain s 1);
      Alcotest.(check (list string))
        "deadline and insertion order" [ "first"; "second"; "late" ] (Store.state s).fired)

let stopping_a_driven_timer_prevents_new_timers () =
  let clock = Clock.Manual.create 0 in
  let timer, control = Timer.simulated clock in
  let p = protocol () in
  let _, inbox = Event_bus.create p ~worker_capacity:1 in
  let s = Store.create_exn p ~state:empty ~services:timer inbox in
  let alarms = mount p in
  set s alarms "cancelled" 1;
  Timer.stop timer;
  Timer.stop timer;
  set s alarms "after stop" 0;
  Clock.Manual.advance clock ms;
  Alcotest.(check int) "no pending timers" 0 (Timer.Control.pending control);
  Alcotest.(check (option int)) "no deadline" None (Timer.Control.next_deadline control);
  Alcotest.(check int) "nothing fired" 0 (Timer.Control.fire_due control);
  Alcotest.(check int) "nothing delivered" 0 (drain s 1);
  Store.close s

(* A due timer whose Store is gone cannot be delivered: it is dropped, and
   [fire_due] counts it with the timers it fired, as the Rust port does. *)
let a_due_timer_on_a_closed_bus_is_dropped_and_counted_as_fired () =
  let clock = Clock.Manual.create 0 in
  let timer, control = Timer.simulated clock in
  let p = protocol () in
  let _, inbox = Event_bus.create p ~worker_capacity:1 in
  let s = Store.create_exn p ~state:empty ~services:timer inbox in
  set s (mount p) "orphaned" 1;
  Store.close s;
  Clock.Manual.advance clock ms;
  Alcotest.(check int) "counted as fired" 1 (Timer.Control.fire_due control);
  Alcotest.(check int) "no pending timers" 0 (Timer.Control.pending control);
  Timer.stop timer

(* Setting timers never blocks, even while the timer thread waits for room
   on a full worker lane: the Store's thread sets them, and only it drains
   the lane. Before the fix, the wake pipe filled after about 65,000 timers
   and the Store's thread blocked on it. *)
let setting_timers_never_blocks_while_the_lane_is_full () =
  let p = protocol () in
  let _sink, inbox = Event_bus.create p ~worker_capacity:1 in
  let timer = Timer.create () in
  let s = Store.create_exn p ~state:empty ~services:timer inbox in
  let alarms = mount p in
  Fun.protect
    ~finally:(fun () ->
      Store.close s;
      Timer.stop timer)
    (fun () ->
      (* Two due at once: the first fills the lane, the second holds the timer
         thread waiting for room. *)
      set s alarms "a" 0;
      set s alarms "b" 0;
      Thread.delay 0.05;
      let reply =
        Option.get (Sink.reply_for (Store.sink s) alarms Timer.fired ~answers:0)
      in
      let request = { Request_id.session = Store.session s; execution = 0; index = 0 } in
      (* A blocked caller cannot fail the test itself: a watchdog does. *)
      let finished = Atomic.make false in
      ignore
        (Thread.create
           (fun () ->
             Thread.delay 30.0;
             if not (Atomic.get finished) then (
               prerr_endline "setting timers blocked for 30s";
               exit 1))
           ());
      let started = Unix.gettimeofday () in
      for _ = 1 to 100_000 do
        Timer.after timer ~delay:(60_000 * ms) reply request
      done;
      Atomic.set finished true;
      let took = Unix.gettimeofday () -. started in
      if took >= 30.0 then Alcotest.failf "setting timers took %.1fs" took;
      let deadline = Unix.gettimeofday () +. 5.0 in
      while List.length (Store.state s).fired < 2 do
        if Unix.gettimeofday () >= deadline then
          Alcotest.fail "the due timers never fired";
        if drain s 16 = 0 then Thread.delay 0.001
      done)

(* A stopped timer leaves no descriptor open, and stopping twice is safe. *)
let stopping_a_timer_closes_its_wake_channel () =
  let next_fd () =
    let fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
    let n : int = Obj.magic fd in
    Unix.close fd;
    n
  in
  let before = if Sys.win32 then None else Some (next_fd ()) in
  for _ = 1 to 20 do
    let timer = Timer.create () in
    Timer.stop timer;
    Timer.stop timer
  done;
  Option.iter
    (fun before -> Alcotest.(check int) "the lowest free descriptor" before (next_fd ()))
    before

(* A wake that lands while the timer thread takes its wake bytes must not be
   swallowed: cleared before the read, the flag stayed set with the socket
   empty, and every later timer slept until an unrelated deadline. *)
let a_wake_while_the_thread_drains_is_not_lost () =
  let agrees, next_writes = Timer.wake_during_drain () in
  Alcotest.(check bool) "the flag agrees with the socket" true agrees;
  Alcotest.(check bool) "the next wake writes" true next_writes

(* A stop on another thread between arming a timer and waking the thread
   could close the socket the wake then wrote to, and the arming raised
   [EBADF]: the stop now waits for the arming to finish. *)
let arming_a_timer_while_it_stops_does_not_raise () =
  Alcotest.(check bool)
    "the stop closed the socket first" false (Timer.stop_during_submit ())

let () =
  Alcotest.run "events.timer"
    [
      ( "timer",
        [
          Alcotest.test_case
            "timers fire in deadline order, equal deadlines in the order set" `Quick
            timers_fire_in_deadline_order_and_equal_deadlines_in_the_order_set;
          Alcotest.test_case "setting timers never blocks while the lane is full" `Quick
            setting_timers_never_blocks_while_the_lane_is_full;
          Alcotest.test_case "stopping a timer closes its wake channel" `Quick
            stopping_a_timer_closes_its_wake_channel;
          Alcotest.test_case "a wake while the thread drains is not lost" `Quick
            a_wake_while_the_thread_drains_is_not_lost;
          Alcotest.test_case "arming a timer while it stops does not raise" `Quick
            arming_a_timer_while_it_stops_does_not_raise;
          Alcotest.test_case "closing the Store stops the timer with alarms still pending"
            `Quick closing_the_store_stops_the_timer_with_alarms_still_pending;
          Alcotest.test_case
            "a simulated timer fires only when its control fires it on the shared clock"
            `Quick
            a_simulated_timer_fires_only_when_its_control_fires_it_on_the_shared_clock;
          Alcotest.test_case "a host clock preserves order across a full lane" `Quick
            a_host_drives_an_arbitrary_clock_and_retries_a_full_lane;
          Alcotest.test_case "a stopped driven timer accepts no new timers" `Quick
            stopping_a_driven_timer_prevents_new_timers;
          Alcotest.test_case "a due timer on a closed bus is dropped and counted as fired"
            `Quick a_due_timer_on_a_closed_bus_is_dropped_and_counted_as_fired;
        ] );
    ]
