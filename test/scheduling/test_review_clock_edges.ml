(* Port of the Rust tests/review_clock_edges.rs.

   Review round 5: the bus closes before Services stop; a fault at the clock
   (IDs running out, a raising clock) faults the Store. *)

open Causet
open Common

(* Raises on its third reading. *)
let failing_clock () =
  let reads = ref 0 in
  {
    Clock.now =
      (fun () ->
        incr reads;
        if not (!reads < 3) then failwith "clock stopped";
        1000 + !reads);
  }

let a_panicking_clock_faults_the_store () =
  let f = fixture_with ~clock:(failing_clock ()) inline 1 in
  check_dispatch "first" (Ok Executed) (Store.dispatch_to f.store f.counter Inc);
  match Store.dispatch_to f.store f.counter Inc with
  | Error (Faulted _) -> ()
  | r -> Alcotest.failf "expected a fault, got %a" pp_dispatch r

let pushed note = Pushed { note }

let pp_send_result f = function
  | Ok () -> Format.pp_print_string f "Ok ()"
  | Error (Full _) -> Format.pp_print_string f "Error (Full _)"
  | Error (Closed ev) -> Format.fprintf f "Error (Closed %a)" download_events.pp ev
  | Error (Wrong_protocol _) -> Format.pp_print_string f "Error (Wrong_protocol _)"
  | Error (Unregistered _) -> Format.pp_print_string f "Error (Unregistered _)"

(* Rust drops the Store, which closes the bus before the Services drop, and
   the Service's [Drop] joins its worker. OCaml has no Drop: [Store.close]
   is the Store going away, and the Service's stop (joining its worker)
   comes after it. *)
let the_bus_closes_before_a_service_joins_its_worker () =
  let joining =
    { fetch = (fun _ _ _ -> failwith "no request is needed for the shutdown test") }
  in
  let f = fixture joining 1 in
  (* No Store input has run, so this fills the bounded worker lane. *)
  (match Sink.try_send_to f.sink f.a download_events (pushed "first") with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the first send fills the lane");
  let outcome = ref None in
  let sink = f.sink and mount = f.a in
  let worker =
    Thread.create
      (fun () ->
        outcome :=
          Some
            (Sink.send_timeout_to sink mount download_events (pushed "second")
               ~timeout:0.1))
      ()
  in
  Store.close f.store;
  (* The Service stops: it joins its worker. *)
  Thread.join worker;
  eq
    ~pp:(fun f -> function
      | None -> Format.pp_print_string f "None" | Some r -> pp_send_result f r)
    "the worker's send"
    (Some (Error (Closed (pushed "second"))))
    !outcome

(* Fails on its third reading: the anchor, then a root Action, then the next
   execution to start. *)
let fails_third () =
  let reads = ref 0 in
  {
    Clock.now =
      (fun () ->
        incr reads;
        if not (!reads < 3) then failwith "clock stopped";
        1000 + !reads);
  }

let a_clock_failure_inside_an_execution_faults_the_store () =
  let f = fixture_with ~clock:(fails_third ()) inline 1 in
  (* Probe runs at 1002; its Effect's clock read fails. *)
  match Store.dispatch_to f.store f.a Probe with
  | Error (Faulted reason) ->
      eq ~pp:Format.pp_print_string "reason" "clock stopped" reason
  | r -> Alcotest.failf "the clock faults the Store, got %a" pp_dispatch r

let () =
  Alcotest.run "review_clock_edges"
    [
      ( "review_clock_edges",
        [
          Alcotest.test_case "a panicking clock faults the store" `Quick
            a_panicking_clock_faults_the_store;
          Alcotest.test_case "the bus closes before a service joins its worker" `Quick
            the_bus_closes_before_a_service_joins_its_worker;
          Alcotest.test_case "a clock failure inside an execution faults the store" `Quick
            a_clock_failure_inside_an_execution_faults_the_store;
        ] );
    ]
