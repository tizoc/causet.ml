(* Port of causet-test's tests/harness.rs: the harness's invariant checks run
   on the starting State and between root inputs, and a failure names the
   node and the root input after which it was found. *)

open Causet
open Causet_test
open Causet_guide.Shop
open Harness_util

let shop_with state limit =
  Node.check
    (Node.of_protocol ~name:"shop" (protocol ()) ~state
       ~services:{ gateway = approve_up_to 100 }
       ~session:1 (Clock.Manual.create 1_000))
    "paid within limit"
    (fun s ->
      match List.find_opt (fun p -> p > limit) s.payments.paid with
      | Some p -> Error (Printf.sprintf "paid %d over %d" p limit)
      | None -> Ok ())

let shop limit = shop_with empty limit
let payments_path = Path.child (Path.root shop_root) payments_slot
let cart_path = Path.child (Path.root shop_root) cart_slot

let only = function
  | [ e ] -> e
  | l -> Alcotest.failf "expected one held Event, got %d" (List.length l)

let a_broken_invariant_is_reported_after_the_root_input_that_broke_it_not_later () =
  let message =
    fails (fun () ->
        let shop = shop 5 in
        let payments = Node.mount shop payments_path in
        ignore (Node.dispatch shop payments (Charge { amount = 3 }));
        (* 1 *)
        Node.deliver shop (only (Node.held shop)).held_id;
        (* 2 *)
        ignore (Node.dispatch shop payments (Charge { amount = 7 }));
        (* 3: the gateway approves, nothing paid yet *)
        Node.deliver shop (only (Node.held shop)).held_id (* 4: paid *))
  in
  Alcotest.(check bool)
    message true
    (contains
       ~sub:
         "node shop, after root input 4: invariant \"paid within limit\": paid 7 over 5"
       message)

let a_starting_state_is_checked_when_the_check_is_added () =
  let state = { empty with payments = { empty.payments with paid = [ 9 ] } } in
  let message = fails (fun () -> ignore (shop_with state 5)) in
  Alcotest.(check bool)
    message true
    (contains
       ~sub:
         "node shop, before any root input: invariant \"paid within limit\": paid 9 over \
          5"
       message)

let a_scenario_reports_its_seed_and_last_steps () =
  let shop = shop 100 in
  let cart = Node.mount shop cart_path in
  ignore (Node.dispatch shop cart (Add { item = "tea"; price = 4 }));
  ignore (Node.dispatch shop cart Checkout);
  let scenario = Scenario.create 42 1_000 in
  let nodes = [ driven shop ] in
  Alcotest.(check bool)
    "quiet" true
    (Scenario.run_until scenario nodes no_world ~steps:10 ~until:(fun () -> false) = Quiet);
  Alcotest.(check (option int)) "seed" (Some 42) (Scenario.seed scenario);
  Alcotest.(check string)
    "report"
    "scenario seed 42 failed after 1 steps:\n\
    \     1. shop: deliver payments.charged to /payments"
    (Scenario.report scenario 5);
  List.iter (Scenario.note scenario) (List.init 30 (Printf.sprintf "step %d"));
  let report = Scenario.report scenario 2 in
  Alcotest.(check bool)
    report true
    (starts_with ~prefix:"scenario seed 42 failed after 31 steps; the last 2:" report);
  Alcotest.(check bool)
    report true
    (ends_with ~suffix:"    30. step 28\n    31. step 29" report)

(* Rust checks that a Scenario's report-on-drop does not swallow the panic.
   OCaml has no drop, so a live scenario simply lets the exception pass. *)
let a_scenario_alive_in_a_failing_test_lets_the_panic_through () =
  let message =
    fails (fun () ->
        let _scenario = Scenario.create 3 1_000 in
        failwith "a failed check")
  in
  Alcotest.(check bool) message true (contains ~sub:"a failed check" message)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "harness"
    [
      ( "harness",
        [
          t "a broken invariant is reported after the root input that broke it, not later"
            a_broken_invariant_is_reported_after_the_root_input_that_broke_it_not_later;
          t "a starting state is checked when the check is added"
            a_starting_state_is_checked_when_the_check_is_added;
          t "a scenario reports its seed and last steps"
            a_scenario_reports_its_seed_and_last_steps;
          t "a scenario alive in a failing test lets the panic through"
            a_scenario_alive_in_a_failing_test_lets_the_panic_through;
        ] );
    ]
