(* Timer edges found in review: overdue timers keep their deadline order
   after the worker lane was full (the Rust port's
   tests/review_timer_edges.rs). *)

open Causet
open Alarms

let overdue_timers_keep_deadline_order_after_the_worker_lane_was_full () =
  let p = protocol () in
  let m = mount p in
  let sink, inbox = Event_bus.create p ~worker_capacity:1 in
  let timer = Timer.create () in
  let store = Store.create_exn p ~state:empty ~services:timer inbox in
  Fun.protect
    ~finally:(fun () ->
      Store.close store;
      Timer.stop timer)
    (fun () ->
      set store m "filler" 60_000;
      let request = fst (List.hd (Store.state store).tags) in
      (match
         Downloads.in_thread (fun () -> Sink.try_send_to sink m Timer.fired { request })
       with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "the filler should take the worker lane");
      set store m "blocker" 0;

      (* Observe a failed send, without removing any Event, so the timer
         worker is known to be blocked delivering "blocker" before more timers
         arrive. *)
      let rejected = Atomic.make 0 in
      Store.add_observer store
        {
          Observe.none with
          bus = (fun d -> ignore (Atomic.fetch_and_add rejected d.rejected_full));
        };
      let bound = Unix.gettimeofday () +. 2.0 in
      while Atomic.get rejected = 0 do
        ignore (drain store 0);
        if Unix.gettimeofday () >= bound then
          Alcotest.fail "the timer did not wait on the full bus";
        Thread.delay 0.001
      done;

      set store m "late" 100;
      set store m "early" 0;
      Thread.delay 0.15;

      let bound = Unix.gettimeofday () +. 2.0 in
      while List.length (Store.state store).fired < 4 do
        ignore (drain store 1);
        if Unix.gettimeofday () >= bound then
          Alcotest.failf "timers did not finish: [%s]"
            (String.concat "; " (Store.state store).fired);
        Thread.delay 0.001
      done;
      Alcotest.(check (list string))
        "fired"
        [ "filler"; "blocker"; "early"; "late" ]
        (Store.state store).fired)

(* A deadline years off is waited for in native-sized chunks, so the wait's
   timeout conversion cannot end the timer thread before a later timer fires. *)
let a_timer_years_off_leaves_the_timer_working () =
  let p = protocol () in
  let m = mount p in
  let _, inbox = Event_bus.create p ~worker_capacity:16 in
  let timer = Timer.create () in
  let store = Store.create_exn p ~state:empty ~services:timer inbox in
  Fun.protect
    ~finally:(fun () ->
      Store.close store;
      Timer.stop timer)
    (fun () ->
      set store m "in five years" (5 * 365 * 86_400 * 1_000);
      Thread.delay 0.02;
      set store m "soon" 10;
      let bound = Unix.gettimeofday () +. 2.0 in
      while (Store.state store).fired = [] do
        ignore (drain store 16);
        if Unix.gettimeofday () >= bound then Alcotest.fail "the timer stopped firing";
        Thread.delay 0.001
      done;
      Alcotest.(check (list string)) "fired" [ "soon" ] (Store.state store).fired)

let () =
  Alcotest.run "events.review_timer_edges"
    [
      ( "review timer edges",
        [
          Alcotest.test_case
            "overdue timers keep deadline order after the worker lane was full" `Quick
            overdue_timers_keep_deadline_order_after_the_worker_lane_was_full;
          Alcotest.test_case "a timer years off leaves the timer working" `Quick
            a_timer_years_off_leaves_the_timer_working;
        ] );
    ]
