(* Port of causet-test's tests/invariants.rs: node invariants on the Store,
   their keys, their memory across a restart, and groups of nodes whose
   published views shared checks read. *)

open Causet
open Causet_test
open Harness_util

type set = Set of int

let set_actions = Message.make ~name:"level.action" ~kind:(fun _ -> "level.set") ()

let level_comp =
  Component.make ~name:"level" ~actions:set_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Set n) _ ->
      if n = 8 then Bug.condition __POS__ "eight";
      Ctx.set_state ctx n)
    ()

let level name session =
  Node.create ~name level_comp ~state:0 ~services:() ~session (Clock.Manual.create 1_000)

let set node n =
  let mount = Node.mount node (Path.root level_comp) in
  ignore (Node.dispatch node mount (Set n))

let quick = { default with runs = 50 }

(* ------------------------------------------------------------------------ *)
(* Node invariants *)

let allow_bugs_does_not_silence_an_invariant () =
  let node =
    Node.check
      (Node.allow_bugs (level "level" 1))
      "below ten"
      (fun n -> if n < 10 then Ok () else Error (string_of_int n))
  in
  set node 8;
  (* a bug report, allowed *)
  let failure = fails (fun () -> set node 12) in
  Alcotest.(check string)
    "failure" "node level, after root input 2: invariant \"below ten\": 12" failure

let a_violation_and_a_panicking_check_have_their_own_keys () =
  let violated =
    found_exn
      (search quick (fun rng ->
           let node =
             Node.check (level "level" 1) "below ten" (fun n ->
                 if n < 10 then Ok () else Error (string_of_int n))
           in
           set node (Rng.below rng 20)))
  in
  Alcotest.(check string) "key" "node level: invariant \"below ten\"" violated.key;
  Alcotest.check ints "choices" [ 10 ] violated.choices;
  let panicked =
    found_exn
      (search quick (fun rng ->
           let node =
             Node.check (level "level" 1) "fragile" (fun n ->
                 if n >= 10 then failwith (Printf.sprintf "cannot read %d" n);
                 Ok ())
           in
           set node (Rng.below rng 20)))
  in
  Alcotest.(check string) "key" "node level: invariant \"fragile\" panicked" panicked.key

let an_invariant_keeps_its_memory_and_hears_of_a_restart_first () =
  let never_decreases () =
    let most = ref 0 in
    fun n step ->
      if step = Restored then most := n;
      (* this application's policy: the restored State is the new floor *)
      let broken = n < !most in
      most := max !most n;
      if broken then Error (Printf.sprintf "%d after %d" n !most) else Ok ()
  in
  let node = Node.check_with (level "level" 1) "never decreases" (never_decreases ()) in
  set node 5;
  (* Restored lower: told first, so no violation. *)
  Node.restart node ~state:2 ~services:() ~session:2;
  set node 3;
  (* Its memory came along: 3 is the floor now. *)
  let failure = fails (fun () -> set node 1) in
  Alcotest.(check string)
    "failure" "node level, after root input 3: invariant \"never decreases\": 1 after 3"
    failure

(* ------------------------------------------------------------------------ *)
(* Groups *)

(* "b never above a", with every step it ran at, by name. *)
let ordered group =
  let seen = ref [] in
  Group.check group "b never above a" (fun views step ->
      seen :=
        !seen
        @ [
            (match step with
            | Added -> "added"
            | Published node -> node ^ " published"
            | Settled node -> node ^ " settled"
            | Restarted node -> node ^ " restarted");
          ];
      match (Group.view views "a", Group.view views "b") with
      | Some a, Some b when b > a -> Error (Printf.sprintf "b %d > a %d" b a)
      | _ -> Ok ());
  seen

let a_group_check_runs_on_publish_and_after_every_member_s_root_input () =
  let group = Group.create "pair" in
  let seen = ordered group in
  let a = Node.publish (level "a" 1) group Fun.id in
  let b = Node.publish (level "b" 2) group Fun.id in
  set a 5;
  set b 3;
  Node.restart b ~state:1 ~services:() ~session:3;
  Alcotest.(check (list string))
    "steps"
    [ "added"; "a published"; "b published"; "a settled"; "b settled"; "b restarted" ]
    !seen

(* The second half of the Rust test above: a missing member is None, so once
   [a] is gone nothing bounds [b]. In Rust, dropping a node removes its views
   from its groups; the OCaml harness has no way for a node to leave (no
   drop, no [Group.leave]/[Node.close]), so [a]'s view and [b]'s name stay. *)
let a_member_that_is_gone_no_longer_bounds_the_others () =
  let group = Group.create "pair" in
  let _seen = ordered group in
  let a = Node.publish (level "a" 1) group Fun.id in
  let b = Node.publish (level "b" 2) group Fun.id in
  set a 5;
  set b 3;
  Node.restart b ~state:1 ~services:() ~session:3;
  (* Rust: drop(a); drop(b). *)
  Node.close a;
  Node.close b;
  let b = Node.publish (level "b" 4) group Fun.id in
  set b 9

let a_group_failure_names_the_group_and_when () =
  let group = Group.create "pair" in
  let _seen = ordered group in
  let a = Node.publish (level "a" 1) group Fun.id in
  let b = Node.publish (level "b" 2) group Fun.id in
  set a 5;
  let failure = fails (fun () -> set b 6) in
  Alcotest.(check string)
    "failure"
    "group pair, after a root input of b: invariant \"b never above a\": b 6 > a 5"
    failure

let group_keys_tell_two_groups_apart () =
  let found =
    found_exn
      (search quick (fun rng ->
           let left = Group.create "left" and right = Group.create "right" in
           let _l = ordered left in
           let _r = ordered right in
           let a =
             Node.publish (Node.publish (level "a" 1) left Fun.id) right (fun n -> n + 10)
           in
           let b = Node.publish (Node.publish (level "b" 2) left Fun.id) right Fun.id in
           set a (Rng.below rng 20);
           set b (Rng.below rng 20)))
  in
  Alcotest.(check string) "key" "group left: invariant \"b never above a\"" found.key;
  Alcotest.check ints "a = 0, then b = 1: above a on the left only" [ 0; 1 ] found.choices

let a_node_publishes_once_per_group () =
  let group = Group.create "pair" in
  let _a = Node.publish (level "a" 1) group Fun.id in
  let failure = fails (fun () -> ignore (Node.publish (level "a" 2) group Fun.id)) in
  Alcotest.(check string) "failure" "group pair: node \"a\" has already published" failure

let a_node_in_two_groups_has_each_group_check_once_on_its_new_view () =
  let left = Group.create "left" and right = Group.create "right" in
  let latest = ref (0, 0) in
  let runs = ref 0 in
  List.iter
    (fun (group, mine) ->
      Group.check group "views agree" (fun views step ->
          match Group.view views "a" with
          | None -> Ok ()
          | Some n ->
              (match step with
              | Settled _ ->
                  incr runs;
                  let l, r = !latest in
                  latest := if mine = 0 then (n, r) else (l, n)
              | _ -> ());
              Ok ()))
    [ (left, 0); (right, 1) ];
  let a = Node.publish (Node.publish (level "a" 1) left Fun.id) right Fun.id in
  set a 4;
  Alcotest.(check (pair (pair int int) int))
    "once per group, each on the new view"
    ((4, 4), 2)
    (!latest, !runs)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "invariants"
    [
      ( "node",
        [
          t "allow_bugs does not silence an invariant"
            allow_bugs_does_not_silence_an_invariant;
          t "a violation and a panicking check have their own keys"
            a_violation_and_a_panicking_check_have_their_own_keys;
          t "an invariant keeps its memory and hears of a restart first"
            an_invariant_keeps_its_memory_and_hears_of_a_restart_first;
        ] );
      ( "groups",
        [
          t "a group check runs on publish and after every member's root input"
            a_group_check_runs_on_publish_and_after_every_member_s_root_input;
          t "a member that is gone no longer bounds the others"
            a_member_that_is_gone_no_longer_bounds_the_others;
          t "a group failure names the group and when"
            a_group_failure_names_the_group_and_when;
          t "group keys tell two groups apart" group_keys_tell_two_groups_apart;
          t "a node publishes once per group" a_node_publishes_once_per_group;
          t "a node in two groups has each group check once on its new view"
            a_node_in_two_groups_has_each_group_check_once_on_its_new_view;
        ] );
    ]
