(* Ported from the Rust port's [tests/review_observer_edges.rs]: an observer
   fault faults the Store at its own boundary; clock faults while starting a
   synchronous Effect child fault it too. (Rust's Stats test is skipped: the
   OCaml port has no Stats observer, and the test is about [&'static str]
   pointer identity, which OCaml strings do not have.) *)

open Causet
open Common

let manual at = Clock.Manual.clock (Clock.Manual.create at)

let expect_fault msg expected = function
  | Error (Faulted m) -> Alcotest.(check string) msg expected m
  | Ok _ ->
      Alcotest.failf "%s: expected the Store to fault with %S, it did not" msg expected
  | Error _ -> Alcotest.failf "%s: expected a fault, got another error" msg

let an_observer_fault_faults_the_store_at_its_boundary () =
  let f = fixture_with ~clock:(manual 1000) inline_service 4 in
  Store.add_observer f.store
    { Observe.none with action_finish = (fun _ -> failwith "observer broke") };
  expect_fault "dispatch" "observer broke" (Store.dispatch_to f.store f.counter Inc);
  Alcotest.(check int) "counter" 1 (Store.state f.store).counter.n

let a_child_start_observer_fault_stops_before_the_child_runs () =
  let f = fixture_with ~clock:(manual 1000) inline_service 4 in
  Store.add_observer f.store
    {
      Observe.none with
      action_start =
        (fun (exec : Observe.Execution.t) _ ->
          if exec.kind = "downloader.note" then failwith "child start observer broke");
    };
  expect_fault "dispatch" "child start observer broke"
    (Store.dispatch_to f.store f.a Probe);
  Alcotest.(check (list int)) "no notes" [] (Store.state f.store).a.notes

(* Fails on its fourth reading. *)
let fails_fourth () =
  let reads = ref 0 in
  {
    Clock.now =
      (fun () ->
        incr reads;
        if not (!reads < 4) then failwith "clock stopped";
        1000 + !reads);
  }

let a_clock_fault_starting_a_synchronous_effect_child_faults () =
  let f = fixture_with ~clock:(fails_fourth ()) inline_service 4 in
  (* Anchor, root Probe, its Effect, then the admitted synchronous Note 1. *)
  expect_fault "dispatch" "clock stopped" (Store.dispatch_to f.store f.a Probe)

let an_effect_finish_observer_fault_comes_after_the_synchronous_children () =
  let f = fixture_with ~clock:(manual 1000) inline_service 4 in
  Store.add_observer f.store
    {
      Observe.none with
      effect_finish = (fun _ -> failwith "effect finish observer broke");
    };
  Alcotest.(check bool)
    "faulted" true
    (Result.is_error (Store.dispatch_to f.store f.a Probe));
  Alcotest.(check (list int))
    "the child ran before the Effect finished" [ 1 ] (Store.state f.store).a.notes

let a_bus_observer_fault_faults_the_store () =
  let f = fixture_with ~clock:(manual 1000) inline_service 4 in
  ignore (Store.dispatch_exn f.store f.counter Inc);
  Store.add_observer f.store
    { Observe.none with bus = (fun _ -> failwith "bus observer broke") };
  expect_fault "drain" "bus observer broke" (Store.drain_events f.store 16)

(* Each observer is called at the boundaries it has a callback of its own
   for, in the order added; a callback left as [Observe.none]'s is never
   called, and a boundary nobody listens to is skipped whole. *)
let each_observer_is_called_at_its_own_callbacks () =
  let f = fixture_with ~clock:(manual 1000) inline_service 4 in
  let calls = ref [] in
  let call c = calls := c :: !calls in
  Store.add_observer f.store
    {
      Observe.none with
      action_start = (fun _ _ -> call "a start");
      action_settled = (fun _ -> call "a settled");
    };
  Store.add_observer f.store
    { Observe.none with action_finish = (fun _ -> call "b finish") };
  Store.add_observer f.store Observe.none;
  ignore (Store.dispatch_exn f.store f.counter Inc);
  Alcotest.(check (list string))
    "calls"
    [ "a start"; "b finish"; "a settled" ]
    (List.rev !calls)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "review_observer_edges"
    [
      ( "observer edges",
        [
          t "each observer is called at its own callbacks"
            each_observer_is_called_at_its_own_callbacks;
          t "an observer fault faults the store at its boundary"
            an_observer_fault_faults_the_store_at_its_boundary;
          t "a child start observer fault stops before the child runs"
            a_child_start_observer_fault_stops_before_the_child_runs;
          t "a clock fault starting a synchronous effect child faults"
            a_clock_fault_starting_a_synchronous_effect_child_faults;
          t "an effect finish observer fault comes after the synchronous children"
            an_effect_finish_observer_fault_comes_after_the_synchronous_children;
          t "a bus observer fault faults the store" a_bus_observer_fault_faults_the_store;
        ] );
    ]
