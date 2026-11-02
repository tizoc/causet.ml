(* The held bus: every accepted send waits until the test releases or
   discards it; a held send never fails for room, a release into a full
   worker lane does; closing the Store drops what is held (the Rust port's
   tests/held.rs). *)

open Causet
open Causet_guide.Shop

let send_result, drained, in_thread, ok_dispatch =
  Downloads.(send_result, drained, in_thread, ok_dispatch)

type held_fixture = {
  store : (shop_state, shop_services) Store.t;
  sink : Sink.t;
  held : Held.t;
  payments : (payments_state, gateway, payments_action, payments_effect, never) mount;
}

let held worker_capacity =
  let p = protocol () in
  let sink, inbox, held = Event_bus.held p ~worker_capacity 9 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      p ~state:empty
      ~services:{ gateway = approve_up_to 100 }
      inbox
  in
  {
    store;
    sink;
    held;
    payments = Protocol.resolve_exn p (Path.child (Path.root shop_root) payments_slot);
  }

let charged_pp f (c : charged) =
  Format.fprintf f "{ request = %a; approved = %b }" Request_id.pp c.request c.approved

let charged_send_result = send_result charged_pp

(* A gateway's answer sent from another thread, which takes the worker lane. *)
let from_a_worker f index =
  let request = { Request_id.session = 9; execution = 1; index } in
  Alcotest.check charged_send_result "accepted" (Ok ())
    (in_thread (fun () ->
         Sink.try_send_to f.sink f.payments charged_message { request; approved = true }))

let release_result =
  Alcotest.testable
    (fun f -> function
      | Ok () -> Format.pp_print_string f "Ok ()"
      | Error Not_held -> Format.pp_print_string f "Error Not_held"
      | Error Release_full -> Format.pp_print_string f "Error Release_full"
      | Error Release_closed -> Format.pp_print_string f "Error Release_closed")
    ( = )

let lane =
  Alcotest.testable
    (fun f -> function
      | Store_lane -> Format.pp_print_string f "Store_lane"
      | Worker_lane -> Format.pp_print_string f "Worker_lane")
    ( = )

let charge f amount = ok_dispatch f.store f.payments (Charge { amount })

let held_events_wait_and_are_delivered_in_the_order_released () =
  let f = held 4 in
  charge f 1;
  charge f 2;

  (* Both answers are held, in send order, with where and what they answer. *)
  let list = Held.list f.held in
  Alcotest.(check int) "two held" 2 (List.length list);
  let first = List.nth list 0 and second = List.nth list 1 in
  Alcotest.(check bool) "in send order" true (first.held_id < second.held_id);
  List.iter
    (fun (e : held_event) ->
      Alcotest.(check (pair string string))
        "kind, path"
        ("payments.charged", "/payments")
        (e.held_kind, e.held_path);
      Alcotest.check lane "lane" Store_lane e.held_lane;
      Alcotest.(check int) "session" 9 e.held_session;
      Alcotest.(check bool) "answers an Effect" true (Option.is_some e.held_answers))
    list;
  Alcotest.check drained "nothing reaches a lane while held" (Ok 0)
    (Store.drain_events f.store 16);

  (* Released in the opposite order, they are delivered in that order. *)
  Alcotest.check release_result "release second" (Ok ())
    (Held.release f.held second.held_id);
  Alcotest.check release_result "release first" (Ok ())
    (Held.release f.held first.held_id);
  Alcotest.check release_result "released twice" (Error Not_held)
    (Held.release f.held first.held_id);
  Alcotest.check drained "two drained" (Ok 2) (Store.drain_events f.store 16);
  Alcotest.(check (list int)) "paid" [ 2; 1 ] (Store.state f.store).payments.paid

let a_held_send_never_fails_for_room_but_a_release_into_a_full_lane_does () =
  let f = held 1 in
  from_a_worker f 0;
  from_a_worker f 1 (* past the lane's capacity, still accepted *);
  let list = Held.list f.held in
  Alcotest.(check (list lane))
    "worker lanes" [ Worker_lane; Worker_lane ]
    (List.map (fun (e : held_event) -> e.held_lane) list);
  let first = List.nth list 0 and second = List.nth list 1 in

  Alcotest.check release_result "release first" (Ok ())
    (Held.release f.held first.held_id);
  Alcotest.check release_result "release into a full lane" (Error Release_full)
    (Held.release f.held second.held_id);
  Alcotest.(check int) "a refused release stays held" 1 (Held.length f.held);
  Alcotest.check drained "one drained" (Ok 1) (Store.drain_events f.store 1);
  Alcotest.check release_result "release second" (Ok ())
    (Held.release f.held second.held_id);
  Alcotest.check drained "the second drained" (Ok 1) (Store.drain_events f.store 16)

let a_discarded_answer_is_lost_and_inspect_reads_without_releasing () =
  let f = held 4 in
  charge f 500;
  let answer =
    match Held.list f.held with
    | [ answer ] -> answer
    | l -> Alcotest.failf "one held answer, got %d" (List.length l)
  in
  Alcotest.(check (option bool))
    "inspect reads the answer" (Some false)
    (Option.map
       (fun (c : charged) -> c.approved)
       (Held.inspect f.held answer.held_id charged_message));
  Alcotest.(check bool)
    "another Event type" true
    (Option.is_none (Held.inspect f.held answer.held_id Timer.fired));
  Alcotest.(check int) "still held" 1 (Held.length f.held);

  Alcotest.(check bool) "discarded" true (Held.discard f.held answer.held_id);
  Alcotest.(check bool) "discarded once" false (Held.discard f.held answer.held_id);
  Alcotest.check drained "nothing drained" (Ok 0) (Store.drain_events f.store 16);
  Alcotest.(check int)
    "the request waits for an answer that never comes" 1
    (Pending.cardinal (Store.state f.store).payments.pending)

let closing_the_store_drops_what_is_held () =
  let f = held 4 in
  from_a_worker f 0;
  let answer =
    match Held.list f.held with
    | [ answer ] -> answer
    | l -> Alcotest.failf "one held answer, got %d" (List.length l)
  in
  (* Rust: drop(store). *)
  Store.close f.store;
  Alcotest.(check bool) "nothing held" true (Held.is_empty f.held);
  Alcotest.check release_result "release after close" (Error Release_closed)
    (Held.release f.held answer.held_id);
  let request = { Request_id.session = 9; execution = 1; index = 1 } in
  match
    Sink.try_send_to f.sink f.payments charged_message { request; approved = true }
  with
  | Error (Closed _) -> ()
  | r -> Alcotest.failf "expected Closed, got %a" (Alcotest.pp charged_send_result) r

(* A front send from a worker is held for the head of the worker lane, and
   released there only while the lane has room. *)
let a_front_send_from_a_worker_is_held_for_the_head_of_the_worker_lane () =
  let f = held 1 in
  let reply = Option.get (Sink.reply_for f.sink f.payments charged_message ~answers:1) in
  from_a_worker f 0;
  let request = { Request_id.session = 9; execution = 1; index = 1 } in
  Alcotest.check charged_send_result "accepted" (Ok ())
    (in_thread (fun () -> Reply.send_front reply { request; approved = true }));
  let list = Held.list f.held in
  Alcotest.(check (list (pair lane bool)))
    "lanes and fronts"
    [ (Worker_lane, false); (Worker_lane, true) ]
    (List.map (fun (e : held_event) -> (e.held_lane, e.held_front)) list);
  let back = List.nth list 0 and front = List.nth list 1 in
  Alcotest.check release_result "release the back one" (Ok ())
    (Held.release f.held back.held_id);
  Alcotest.check release_result "the worker lane is full, its head included"
    (Error Release_full)
    (Held.release f.held front.held_id);
  Alcotest.check drained "one drained" (Ok 1) (Store.drain_events f.store 1);
  Alcotest.check release_result "release the front one" (Ok ())
    (Held.release f.held front.held_id);
  Alcotest.check drained "the front one drained" (Ok 1) (Store.drain_events f.store 16)

let () =
  Alcotest.run "events.held"
    [
      ( "held",
        [
          Alcotest.test_case "held Events wait and are delivered in the order released"
            `Quick held_events_wait_and_are_delivered_in_the_order_released;
          Alcotest.test_case
            "a held send never fails for room, but a release into a full lane does" `Quick
            a_held_send_never_fails_for_room_but_a_release_into_a_full_lane_does;
          Alcotest.test_case "a discarded answer is lost; inspect reads without releasing"
            `Quick a_discarded_answer_is_lost_and_inspect_reads_without_releasing;
          Alcotest.test_case "closing the Store drops what is held" `Quick
            closing_the_store_drops_what_is_held;
          Alcotest.test_case
            "a front send from a worker is held for the head of the worker lane" `Quick
            a_front_send_from_a_worker_is_held_for_the_head_of_the_worker_lane;
        ] );
    ]
