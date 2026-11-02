(* Port of causet-test's tests/generate_edges.rs: the final replay starts a
   fresh run, a saved case keeps its step limit, and a fault keeps the
   faulting code's place when something the Store notifies raises too,
   before the fault or after it, or the application caught an exception of
   its own, first or in a cleanup while the fault unwinds.

   Rust's [panic!] is [failwith] here, the idiomatic equivalent. *)

open Causet
open Causet_test
open Harness_util

type input =
  | Set of int
  | Crash_a
  | Crash_b
  | Report_then_crash of bool
  | Caught_then_crash of bool
  | Cleanup_then_crash of bool

let inputs =
  Message.make ~name:"generation.edge"
    ~kind:(function
      | Set _ -> "generation.edge.set"
      | Crash_a -> "generation.edge.crash_a"
      | Crash_b -> "generation.edge.crash_b"
      | Report_then_crash _ -> "generation.edge.report_then_crash"
      | Caught_then_crash _ -> "generation.edge.caught_then_crash"
      | Cleanup_then_crash _ -> "generation.edge.cleanup_then_crash")
    ()

let counter =
  Component.make ~name:"counter" ~actions:inputs ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx input _ ->
      match input with
      | Set n -> Ctx.set_state ctx n
      | Crash_a -> failwith "fault A"
      | Crash_b -> failwith "fault B"
      | Report_then_crash b ->
          Bug.condition __POS__ "expected diagnostic before a queued child";
          Ctx.emit_action ctx (if b then Crash_b else Crash_a)
      | Cleanup_then_crash b ->
          (* A cleanup that catches its own exception while the fault
             unwinds through it: Rust's destructor, here [Fun.protect]'s
             [finally]. *)
          Fun.protect
            ~finally:(fun () -> try failwith "handled cleanup panic" with _ -> ())
            (fun () -> if b then failwith "fault B" else failwith "fault A")
      | Caught_then_crash b ->
          (* An exception the application catches itself, then a fault. *)
          (try failwith "handled inside the reducer" with _ -> ());
          Ctx.emit_action ctx (if b then Crash_b else Crash_a))
    ()

let node () =
  let node =
    Node.create ~name:"counter" counter ~state:0 ~services:() ~session:1
      (Clock.Manual.create 0)
  in
  Node.check node "not one" (fun state ->
      if state = 1 then Error "one is invalid" else Ok ())

let dispatch node input =
  let mount = Node.mount node (Path.root counter) in
  ignore (Node.dispatch node mount input)

let one_step = { default with runs = 1; max_steps = 1; max_shrinks = 0 }

let final_check_replay_starts_with_a_fresh_step_counter () =
  let failed =
    fails (fun () -> check ~config:one_step (fun _ -> dispatch (node ()) (Set 1)))
  in
  Alcotest.(check bool)
    ("wrong final failure: " ^ failed)
    true
    (contains ~sub:"invariant \"not one\"" failed)

(* Rust's [Found::replay]: the OCaml harness has no such function, so the
   replay applies the limit the failure was found under by hand. *)
let a_saved_budget_failure_repeats_through_public_replay () =
  let property _ =
    let node = node () in
    dispatch node (Set 2);
    dispatch node (Set 2)
  in
  let found = found_exn (search one_step property) in
  Alcotest.(check string) "key" "step budget" found.key;
  let replayed =
    match
      replay
        ~config:{ default with max_steps = found.found_max_steps }
        found.choices property
    with
    | () -> false
    | exception _ -> true
  in
  Alcotest.(check bool)
    ("the advertised saved failure passed on replay: " ^ found_string found)
    true replayed

let fault_observer_panics =
  { Observe.none with fault = (fun _ -> failwith "ignored observer failure") }

let first_seed_of_b () =
  let rec go s = if Rng.below (Rng.create s) 2 = 1 then s else go (s + 1) in
  go 0

let ignored_fault_observer_panics_do_not_merge_distinct_store_faults () =
  let seed = first_seed_of_b () in
  let config = { default with runs = 1; seed; max_steps = 10; max_shrinks = 50 } in
  let property rng observer =
    let node = node () in
    if observer then Store.add_observer (Node.store node) fault_observer_panics;
    let input = if Rng.below rng 2 = 0 then Crash_a else Crash_b in
    dispatch node input
  in
  let without = found_exn (search config (fun rng -> property rng false)) in
  Alcotest.check ints
    ("the original B fault must stay B: " ^ found_string without)
    [ 1 ] without.choices;
  let with_ = found_exn (search config (fun rng -> property rng true)) in
  Alcotest.check ints
    ("an ignored observer changed fault identity: " ^ found_string with_)
    without.choices with_.choices

let a_bare_replay_still_has_a_step_limit () =
  let endless =
    fails (fun () ->
        replay [] (fun _ ->
            let node = node () in
            while true do
              dispatch node (Set 2)
            done))
  in
  (* Rust's message names the knob, Config::max_steps; the port's is the
     config's [max_steps] field. *)
  Alcotest.(check bool) endless true (contains ~sub:"max_steps" endless)

let bug_observer_panics =
  { Observe.none with bug = (fun _ -> failwith "ignored report observer failure") }

(* Searches [property] on a seed whose first choice is 1 (fault B), with and
   without [noise], and checks both keep fault B. *)
let keeps_fault_b noise input =
  let seed = first_seed_of_b () in
  let config = { default with runs = 1; seed; max_steps = 10; max_shrinks = 50 } in
  let property rng noisy =
    let node = Node.allow_bugs (node ()) in
    if noisy then noise node;
    dispatch node (input (Rng.below rng 2 <> 0))
  in
  let quiet = found_exn (search config (fun rng -> property rng false)) in
  Alcotest.check ints
    ("the original B fault must stay B: " ^ found_string quiet)
    [ 1 ] quiet.choices;
  let noisy = found_exn (search config (fun rng -> property rng true)) in
  Alcotest.(check string) "key" quiet.key noisy.key;
  Alcotest.check ints
    ("an unrelated panic replaced the real fault key: " ^ found_string noisy)
    quiet.choices noisy.choices

let an_ignored_panic_before_the_fault_does_not_merge_distinct_store_faults () =
  keeps_fault_b
    (fun node -> Store.add_observer (Node.store node) bug_observer_panics)
    (fun b -> Report_then_crash b)

let a_panic_the_application_caught_does_not_merge_distinct_store_faults () =
  keeps_fault_b ignore (fun b -> Caught_then_crash b)

let a_panic_a_destructor_caught_while_the_fault_unwound_does_not_merge_faults () =
  keeps_fault_b ignore (fun b -> Cleanup_then_crash b)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "generate_edges"
    [
      ( "edges",
        [
          t "final check replay starts with a fresh step counter"
            final_check_replay_starts_with_a_fresh_step_counter;
          t "a saved budget failure repeats through public replay"
            a_saved_budget_failure_repeats_through_public_replay;
          t "ignored fault observer panics do not merge distinct store faults"
            ignored_fault_observer_panics_do_not_merge_distinct_store_faults;
          t "a bare replay still has a step limit" a_bare_replay_still_has_a_step_limit;
          t "an ignored panic before the fault does not merge distinct store faults"
            an_ignored_panic_before_the_fault_does_not_merge_distinct_store_faults;
          t "a panic the application caught does not merge distinct store faults"
            a_panic_the_application_caught_does_not_merge_distinct_store_faults;
          t "a panic a destructor caught while the fault unwound does not merge faults"
            a_panic_a_destructor_caught_while_the_fault_unwound_does_not_merge_faults;
        ] );
    ]
