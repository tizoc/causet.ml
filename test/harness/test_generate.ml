(* Port of causet-test's tests/generate.rs: generated runs and shrinking: the
   stream of choices, failure keys, the limits, and what [check] reports. *)

open Causet
open Causet_test
open Harness_util

(* ------------------------------------------------------------------------ *)
(* A number that two invariants watch, and that can fault or report a bug. *)

type set = Set of int

let set_actions =
  Message.make ~name:"number.action"
    ~kind:(fun _ -> "number.set")
    ~pp:(fun f (Set n) -> Format.fprintf f "Set(%d)" n)
    ()

let number_comp =
  Component.make ~name:"number" ~actions:set_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Set n) _ ->
      if n = 14 then failwith "fourteen";
      if n = 8 then
        Bug.condition ~fields:[ ("n", string_of_int n) ] __POS__ "eight is unlucky";
      Ctx.set_state ctx n)
    ()

let number () =
  let node =
    Node.create ~name:"number" number_comp ~state:0 ~services:() ~session:1
      (Clock.Manual.create 1_000)
  in
  let node =
    Node.check node "high" (fun n ->
        if n >= 50 then Error (Printf.sprintf "%d is high" n) else Ok ())
  in
  Node.check node "odd" (fun n ->
      if n mod 2 = 1 then Error (Printf.sprintf "%d is odd" n) else Ok ())

let set node n =
  let mount = Node.mount node (Path.root number_comp) in
  ignore (Node.dispatch node mount (Set n))

let quick = { default with runs = 200 }

(* ------------------------------------------------------------------------ *)
(* The stream of choices *)

let a_replayed_list_is_clamped_to_each_bound_then_zero () =
  let drawn = ref [] in
  replay [ 7; 99; 3 ] (fun rng ->
      let a = Rng.below rng 10 in
      let b = Rng.below rng 5 in
      let c = Rng.below rng 1 in
      let d = Rng.below rng 4 in
      let e = Rng.below rng 4 in
      drawn := [ a; b; c; d; e ]);
  (* 99 clamps to 4; a draw from one value is no choice; past the end, 0. *)
  Alcotest.check ints "drawn" [ 7; 4; 0; 3; 0 ] !drawn

let a_choice_of_zero_is_the_simplest_answer () =
  replay [] (fun rng ->
      Alcotest.(check bool) "more" false (Rng.more rng);
      Alcotest.(check bool) "chance" false (Rng.chance rng 99 100);
      Alcotest.(check string) "pick" "first" (Rng.pick rng [| "first"; "second" |]);
      Alcotest.(check int) "between" 5 (Rng.between rng 5 9))

let a_split_takes_one_choice_and_records_nothing () =
  replay [ 5; 2 ] (fun rng ->
      let world = Rng.split rng in
      let drawn = List.init 3 (fun _ -> Rng.below world 1_000) in
      Alcotest.(check int) "the world's draws are not the run's" 2 (Rng.below rng 10);
      Alcotest.(check (option ints)) "run choices" (Some [ 5; 2 ]) (Rng.choices rng);
      Alcotest.(check (option ints)) "world choices" None (Rng.choices world);
      (* The world is the seed's: replaying the choice replays it. *)
      let same = Rng.create 5 in
      let again = List.init 3 (fun _ -> Rng.below same 1_000) in
      Alcotest.check ints "same world" again drawn)

let the_world_does_not_shift_the_steps () =
  let steps world_draws =
    let rng = Rng.create 9 in
    let world = Rng.split rng in
    List.init 5 (fun _ ->
        let s = Rng.below rng 100 in
        for _ = 1 to world_draws do
          ignore (Rng.below world 100)
        done;
        s)
  in
  Alcotest.check ints "steps" (steps 0) (steps 7)

type kind = Send | Deliver | Restart

let kind_string = function Send -> "Send" | Deliver -> "Deliver" | Restart -> "Restart"
let kinds_t = Alcotest.(list (testable (Fmt.of_to_string kind_string) ( = )))
let kind_t = Alcotest.testable (Fmt.of_to_string kind_string) ( = )
let kinds = [ (Send, 2); (Deliver, 1); (Restart, 1) ]

let a_swarm_of_zero_choices_has_every_kind_and_picks_the_first () =
  replay [] (fun rng ->
      let swarm = Swarm.create rng kinds in
      Alcotest.check kinds_t "kinds" [ Send; Deliver; Restart ] (Swarm.kinds swarm);
      Alcotest.check kind_t "pick" Send (Swarm.pick swarm rng))

let a_swarm_picks_only_the_kinds_it_drew () =
  replay [ 1; 0; 1; 7; 3 ] (fun rng ->
      let swarm = Swarm.create rng kinds in
      Alcotest.check kinds_t "kinds" [ Deliver ] (Swarm.kinds swarm);
      Alcotest.check kind_t "pick" Deliver (Swarm.pick swarm rng);
      Alcotest.check kind_t "pick" Deliver (Swarm.pick swarm rng))

let a_swarm_with_every_kind_off_has_them_all () =
  replay [ 1; 1; 1 ] (fun rng ->
      let swarm = Swarm.create rng kinds in
      Alcotest.(check int) "count" 3 (List.length (Swarm.kinds swarm)))

let a_swarm_picks_by_weight () =
  replay [] (fun rng ->
      let swarm = Swarm.create rng kinds in
      let seeded = Rng.create 4 in
      let sends = ref 0 in
      for _ = 1 to 4_000 do
        if Swarm.pick swarm seeded = Send then incr sends
      done;
      Alcotest.(check bool)
        (Printf.sprintf "%d of 4,000" !sends)
        true
        (!sends >= 1_800 && !sends < 2_200))

(* Regression: weights whose total overflows are refused when the swarm is
   made, not wrapped or panicked on when it picks. Rust's bound is u64::MAX;
   an OCaml int's is max_int, so two weights of 2^61 overflow it. *)
let a_swarm_refuses_weights_that_overflow () =
  let message =
    fails (fun () ->
        replay [] (fun rng -> ignore (Swarm.create rng [ (0, 1 lsl 61); (1, 1 lsl 61) ])))
  in
  Alcotest.(check bool)
    ("refused when made: " ^ message)
    true
    (contains ~sub:"a swarm's weights must total at most" message)

(* Regression: the largest total that fits reaches its last kind (max_int
   for u64::MAX). *)
let a_swarm_of_the_largest_total_picks_its_last_kind () =
  replay
    [ 0; 0; max_int - 1 ]
    (fun rng ->
      let swarm = Swarm.create rng [ (0, max_int - 1); (1, 1) ] in
      Alcotest.(check int) "last kind" 1 (Swarm.pick swarm rng))

(* Rust's clones share one stream; an OCaml Rng.t is shared by aliasing. *)
let clones_draw_from_one_stream () =
  let a = Rng.create 3 in
  let b = a in
  let first = Rng.next a in
  let second = Rng.next b in
  let fresh = Rng.create 3 in
  let f1 = Rng.next fresh in
  let f2 = Rng.next fresh in
  Alcotest.(check (pair int int)) "one stream" (f1, f2) (first, second)

(* The same seed draws the same stream in every port, a split generator's
   included: these numbers are the Rust and TypeScript ports' too. Each row: a
   seed, its first three [next], the seed its [split] draws, that child's two
   [next], [below 10] and [below 1000], and the parent's next [next] after the
   split. *)
let a_seed_draws_the_same_stream_in_every_port () =
  List.iter
    (fun (seed, nexts, split_seed, child, after) ->
      let name what = Printf.sprintf "seed %d: %s" seed what in
      let rng = Rng.create seed in
      let n1 = Rng.next rng in
      let n2 = Rng.next rng in
      let n3 = Rng.next rng in
      Alcotest.(check (list int)) (name "next") nexts [ n1; n2; n3 ];
      let world = Rng.split rng in
      Alcotest.(check (option int)) (name "split seed") (Some split_seed) (Rng.seed world);
      let c1 = Rng.next world in
      let c2 = Rng.next world in
      let c3 = Rng.below world 10 in
      let c4 = Rng.below world 1000 in
      Alcotest.(check (list int)) (name "child") child [ c1; c2; c3; c4 ];
      Alcotest.(check int) (name "after the split") after (Rng.next rng))
    [
      ( 0,
        [ 184964832154816; 6929580258060647; 1228259715532165 ],
        3299258355452336,
        [ 1556177596879896; 3079948183007717; 8; 526 ],
        7187964147299700 );
      ( 1,
        [ 2865243701272905; 3251949076935262; 5527171857800482 ],
        429432007478425,
        [ 4151588442903439; 1578455540222732; 3; 225 ],
        7693114567407942 );
      ( 42,
        [ 6529064058451075; 4471935826588234; 5180173260398988 ],
        8129784920598100,
        [ 6074493261542752; 4401473988561057; 8; 573 ],
        7978443526906943 );
      ( 3735928559,
        [ 8929200111742449; 6872159228071956; 8932636711779373 ],
        1915845255195443,
        [ 8611885385275428; 5148353159110923; 9; 640 ],
        7467217096916731 );
      ( 9007199254740991,
        [ 7124478592440004; 1111579265331998; 1373957957192600 ],
        4568600538254545,
        [ 4916215589202146; 5472395411161787; 4; 847 ],
        7729296003959000 );
    ]

(* ------------------------------------------------------------------------ *)
(* Shrinking keeps to one failure *)

let first_seed p =
  let rec go s = if p s then s else go (s + 1) in
  go 0

let shrinking_does_not_trade_one_invariant_for_another () =
  (* One draw, as the property draws it, picks the first failure: a seed
     whose first run sets a high number. Shrinking lowers it, and 49 would
     still fail, but as "odd": the key keeps it at the smallest high one. *)
  let seed = first_seed (fun s -> Rng.below (Rng.create s) 100 >= 50) in
  let found =
    found_exn
      (search { quick with seed } (fun rng -> set (number ()) (Rng.below rng 100)))
  in
  Alcotest.(check (pair int string))
    "seed and key"
    (seed, "node number: invariant \"high\"")
    (found.seed, found.key);
  Alcotest.check ints "choices" [ 50 ] found.choices

let a_fault_is_keyed_by_where_the_code_panicked () =
  let found =
    found_exn
      (search quick (fun rng ->
           let a = Rng.below rng 10 in
           let b = Rng.below rng 2 in
           set (number ()) ((2 * a) + (14 * b))))
  in
  (* Rust: contains "generate.rs:"; the port's file is test_generate.ml. *)
  Alcotest.(check bool)
    found.key true
    (starts_with ~prefix:"node number: fault at " found.key
    && contains ~sub:"test_generate.ml:" found.key);
  (* The run sets 14 either as 2 * 7 + 0 or as 2 * 0 + 14. From [7, 0] no
     single edit reaches the smaller [0, 1]; the two-choice pass does. *)
  Alcotest.check ints (found_string found) [ 0; 1 ] found.choices

(* A handler that catches the exception of its dispatch, then raises one
   with the same message elsewhere: the fault is the first. *)

type catch = Catch_start | Catch_boom
type run = Run

let catch_actions =
  Message.make ~name:"catcher.action"
    ~kind:(function Catch_start -> "catcher.start" | Catch_boom -> "catcher.boom")
    ()

let run_effects = Message.make ~name:"catcher.effect" ~kind:(fun Run -> "catcher.run") ()

(* Not a tail call, so the backtrace has this line. *)
let boom_line = __LINE__ + 1
let boom () = ignore (failwith "boom" : int)

let catcher =
  Component.make ~name:"catcher" ~actions:catch_actions ~effects:run_effects
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with Catch_start -> Ctx.emit_effect ctx Run | Catch_boom -> boom ())
    ~handle:(fun ctx Run _ ->
      (try ignore (Effect_ctx.dispatch ctx Catch_boom) with Failure _ -> ());
      failwith "boom")
    ()

let a_fault_a_handler_caught_is_keyed_by_the_first_raise () =
  let found =
    found_exn
      (search quick (fun _ ->
           let node =
             Node.create ~name:"catcher" catcher ~state:() ~services:() ~session:1
               (Clock.Manual.create 1)
           in
           let mount = Node.mount node (Path.root catcher) in
           ignore (Node.dispatch node mount Catch_start)))
  in
  Alcotest.(check bool)
    found.key true
    (starts_with ~prefix:"node catcher: fault at " found.key
    && contains ~sub:(Printf.sprintf "test_generate.ml:%d" boom_line) found.key)

let an_unexpected_bug_report_fails_the_node () =
  let found =
    found_exn (search quick (fun rng -> set (number ()) [| 2; 4; 8 |].(Rng.below rng 3)))
  in
  Alcotest.(check bool)
    found.key true
    (starts_with ~prefix:"node number: bug report at " found.key
    && contains ~sub:"test_generate.ml:" found.key);
  Alcotest.check ints "choices" [ 2 ] found.choices;
  (* Expected, it fails nothing, and the test reads it. *)
  let node = Node.allow_bugs (number ()) in
  set node 8;
  Alcotest.(check int) "reports" 1 (List.length (Node.bug_reports node))

let a_property_that_holds_finds_nothing () =
  Alcotest.(check bool)
    "nothing found" true
    (search quick (fun rng -> set (number ()) (10 * Rng.below rng 5)) = None)

(* ------------------------------------------------------------------------ *)
(* Limits *)

let a_generator_that_rejects_everything_fails_the_test () =
  let message = fails (fun () -> check ~config:{ default with runs = 10 } Rng.reject) in
  Alcotest.(check bool)
    message true
    (contains ~sub:"the generator rejects almost everything" message)

let rejected_runs_are_neither_runs_nor_failures () =
  (* Half the runs reject; the rest pass. *)
  Alcotest.(check bool)
    "nothing found" true
    (search quick (fun rng -> if Rng.chance rng 1 2 then Rng.reject rng) = None)

let a_run_that_does_not_end_fails_at_its_step_budget () =
  let found =
    found_exn
      (search { quick with max_steps = 50 } (fun _ ->
           let node = number () in
           while true do
             set node 2
           done))
  in
  Alcotest.(check string) "key" "step budget" found.key

(* ------------------------------------------------------------------------ *)
(* check *)

let check_fails_with_the_shrunk_run_s_own_panic () =
  let seed = first_seed (fun s -> Rng.below (Rng.create s) 100 >= 50) in
  let message =
    fails (fun () ->
        check ~config:{ quick with seed } (fun rng -> set (number ()) (Rng.below rng 100)))
  in
  Alcotest.(check bool)
    message true
    (contains ~sub:"invariant \"high\": 50 is high" message)

let check_says_so_when_a_failure_does_not_repeat () =
  let failed = ref false in
  let message =
    fails (fun () ->
        check ~config:quick (fun _ ->
            let before = !failed in
            failed := true;
            if not before then failwith "fails once only"))
  in
  Alcotest.(check bool)
    message true
    (contains ~sub:"passed when replayed: the run is not repeatable" message)

(* ------------------------------------------------------------------------ *)
(* Determinism *)

(* Bumped by every making of a run: a run that reads it reads what its
   choices do not decide. *)
let made = ref 0

(* Sets the number once, or twice on every other making. *)
let reads_a_counter rng =
  let m = !made in
  incr made;
  let node = number () in
  let n = Rng.below rng 10 * 2 in
  set node n;
  if m mod 2 = 1 then set node n

let a_run_made_differently_from_its_own_choices_is_not_deterministic () =
  made := 0;
  match search { default with runs = 5 } reads_a_counter with
  | None -> Alcotest.fail "not found"
  | Some found ->
      Alcotest.(check (pair string int))
        "key" ("not deterministic", 0) (found.key, found.seed);
      Alcotest.(check (option string))
        "detail"
        (Some "the first made 1 executions, refusals and inline calls, the second 2")
        found.detail

(* Sets the number from the counter, not from the choices: the same kind
   every time, with another value on every making. *)
let sends_a_counted_value _ =
  let m = !made in
  incr made;
  set (number ()) (m mod 2 * 2)

let ends_with ~suffix s =
  let n = String.length suffix and l = String.length s in
  l >= n && String.sub s (l - n) n = suffix

let the_same_kinds_with_other_values_are_not_deterministic () =
  made := 0;
  match search { default with runs = 5 } sends_a_counted_value with
  | None -> Alcotest.fail "not found"
  | Some found ->
      Alcotest.(check string) "key" "not deterministic" found.key;
      let detail = Option.value found.detail ~default:"" in
      Alcotest.(check bool)
        detail true
        (contains ~sub:"at step 1: first " detail && ends_with ~suffix:": Set(2)" detail)

(* Passes on its first making and rejects on the next. *)
let rejects_when_made_again rng =
  let m = !made in
  incr made;
  set (number ()) 2;
  if m mod 2 = 1 then Rng.reject rng

let a_run_that_passes_then_is_rejected_is_not_deterministic () =
  made := 0;
  match search { default with runs = 5 } rejects_when_made_again with
  | None -> Alcotest.fail "not found"
  | Some found ->
      Alcotest.(check string) "key" "not deterministic" found.key;
      Alcotest.(check (option string))
        "detail" (Some "the first passed and the second was rejected") found.detail

let check_fails_a_run_that_is_not_deterministic () =
  made := 0;
  match check ~config:{ default with runs = 5 } reads_a_counter with
  | () -> Alcotest.fail "check passed"
  | exception Failure m ->
      Alcotest.(check bool)
        m true
        (contains ~sub:"is not deterministic: made twice from the same" m)

let the_determinism_check_can_be_turned_off () =
  made := 0;
  Alcotest.(check bool)
    "none" true
    (search { default with runs = 5; determinism = false } reads_a_counter = None)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "generate"
    [
      ( "determinism",
        [
          t "a run made differently from its own choices is not deterministic"
            a_run_made_differently_from_its_own_choices_is_not_deterministic;
          t "the same kinds with other values are not deterministic"
            the_same_kinds_with_other_values_are_not_deterministic;
          t "a run that passes then is rejected is not deterministic"
            a_run_that_passes_then_is_rejected_is_not_deterministic;
          t "check fails a run that is not deterministic"
            check_fails_a_run_that_is_not_deterministic;
          t "the determinism check can be turned off"
            the_determinism_check_can_be_turned_off;
        ] );
      ( "choices",
        [
          t "a replayed list is clamped to each bound then zero"
            a_replayed_list_is_clamped_to_each_bound_then_zero;
          t "a choice of zero is the simplest answer"
            a_choice_of_zero_is_the_simplest_answer;
          t "a split takes one choice and records nothing"
            a_split_takes_one_choice_and_records_nothing;
          t "the world does not shift the steps" the_world_does_not_shift_the_steps;
          t "a swarm of zero choices has every kind and picks the first"
            a_swarm_of_zero_choices_has_every_kind_and_picks_the_first;
          t "a swarm picks only the kinds it drew" a_swarm_picks_only_the_kinds_it_drew;
          t "a swarm with every kind off has them all"
            a_swarm_with_every_kind_off_has_them_all;
          t "a swarm picks by weight" a_swarm_picks_by_weight;
          t "a swarm refuses weights that overflow" a_swarm_refuses_weights_that_overflow;
          t "a swarm of the largest total picks its last kind"
            a_swarm_of_the_largest_total_picks_its_last_kind;
          t "clones draw from one stream" clones_draw_from_one_stream;
          t "a seed draws the same stream in every port"
            a_seed_draws_the_same_stream_in_every_port;
        ] );
      ( "shrinking",
        [
          t "shrinking does not trade one invariant for another"
            shrinking_does_not_trade_one_invariant_for_another;
          t "a fault is keyed by where the code panicked"
            a_fault_is_keyed_by_where_the_code_panicked;
          t "a fault a handler caught is keyed by the first raise"
            a_fault_a_handler_caught_is_keyed_by_the_first_raise;
          t "an unexpected bug report fails the node"
            an_unexpected_bug_report_fails_the_node;
          t "a property that holds finds nothing" a_property_that_holds_finds_nothing;
        ] );
      ( "limits",
        [
          t "a generator that rejects everything fails the test"
            a_generator_that_rejects_everything_fails_the_test;
          t "rejected runs are neither runs nor failures"
            rejected_runs_are_neither_runs_nor_failures;
          t "a run that does not end fails at its step budget"
            a_run_that_does_not_end_fails_at_its_step_budget;
        ] );
      ( "check",
        [
          t "check fails with the shrunk run's own panic"
            check_fails_with_the_shrunk_run_s_own_panic;
          t "check says so when a failure does not repeat"
            check_says_so_when_a_failure_does_not_repeat;
        ] );
    ]
