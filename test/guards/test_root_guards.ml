(* Port of the Rust tests/root_guards.rs.

   Root guards: the application's checks on a mount below the root, reading
   the whole State. They check every Action that enters the mount and the
   root's own inline calls, never an inline call made below the root, and
   run after availability and before the slot guard and the component's own
   check. *)

open Causet
open Guarded

type fixture = {
  store : (app_state, unit) Store.t;
  sink : Sink.t;
  log : Observe.Trace_log.log;
  app_mount : (app_state, unit, app_action, never, never) mount;
  mid_mount : (mid_state, unit, mid_action, never, never) mount;
  other_mount : (mid_state, unit, mid_action, never, never) mount;
  leaf_mount : (leaf_state, unit, leaf_action, leaf_effect, never) mount;
  other_leaf_mount : (leaf_state, unit, leaf_action, leaf_effect, never) mount;
}

let fixture state =
  let reg = Registry.create () in
  Registry.root reg app;
  Registry.admit reg (mid_path ()) mid_may_forward;
  Registry.admit reg (mid_leaf ()) may_add;
  Registry.admit reg (other_leaf ()) other_may_add;
  Registry.admit reg (spare ()) spare_never;
  let proto =
    match Registry.freeze reg with
    | Ok p -> p
    | Error problems -> failwith (String.concat "; " (List.map problem_message problems))
  in
  let sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state ~services:() inbox
  in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let resolve path = Protocol.resolve_exn proto path in
  {
    store;
    sink;
    log;
    app_mount = resolve (Path.root app);
    mid_mount = resolve (mid_path ());
    other_mount = resolve (Path.child (Path.root app) other_slot);
    leaf_mount = resolve (mid_leaf ());
    other_leaf_mount = resolve (other_leaf ());
  }

let blocking blocked = { empty with blocked; limit = 1_000 }
let dispatch f mount action = Store.dispatch_exn f.store mount action
let to_leaf f action = dispatch f f.leaf_mount action
let to_mid f action = dispatch f f.mid_mount action

let bump f n =
  match Sink.try_send_to f.sink f.leaf_mount bumped_message (Bumped n) with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the Event was not sent"

let drain f = ignore (Store.drain_events f.store 16)
let total_of (m : mid_state) = match m.leaf with Some l -> l.total | None -> 0
let total f = total_of (Store.state f.store).mid
let other_total f = total_of (Store.state f.store).other

let sent_name = function
  | Observe.Root -> "root"
  | Observe.Queued _ -> "queued"
  | Observe.From_effect _ -> "effect"
  | Observe.From_inline _ -> "inline"

let by_name = function
  | Observe.Root_guard -> "root-guard"
  | Observe.Guard -> "guard"
  | Observe.Component -> "component"
  | Observe.Absent -> "absent"

(* Every refusal so far: its path, kind, what refused it and where it was
   sent from, as [root], [queued], [effect] or [inline]. *)
let refusals f =
  List.map
    (fun (r : Observe.Refusal_entry.t) ->
      (r.path, r.kind, by_name r.by, sent_name r.sent))
    (Observe.Trace_log.refusals f.log)

let refusal = Alcotest.(list (pair (pair string string) (pair string string)))

let check_refusals name expected f =
  let nest l = List.map (fun (p, k, b, s) -> ((p, k), (b, s))) l in
  Alcotest.check refusal name (nest expected) (nest (refusals f))

let root_guard path sent = (path, "leaf.add", "root-guard", sent)

let outcome_name = function
  | Executed -> "Executed"
  | Disabled -> "Disabled"
  | Faulted -> "Faulted"

let outcome =
  Alcotest.testable (fun f o -> Format.pp_print_string f (outcome_name o)) ( = )

(* Each route by which an Action enters the leaf, with [n]: from outside,
   queued by itself, queued by its parent, adapted from an Event, lowered
   from its child's Output, dispatched by its Effect handler. *)
let every_route f n =
  ignore (to_leaf f (Add n));
  ignore (to_leaf f (Again n));
  ignore (to_mid f (Send (Add n)));
  bump f n;
  drain f;
  ignore (to_leaf f (Ask n));
  ignore (to_leaf f (Echo n))

let a_root_guard_checks_every_action_that_enters_its_mount () =
  let f = fixture (blocking [ 5 ]) in
  every_route f 5;
  Alcotest.(check int) "nothing blocked was added" 0 (total f);
  check_refusals "refusals"
    (List.map (root_guard "/mid/leaf")
       [ "root"; "queued"; "queued"; "queued"; "queued"; "effect" ])
    f;
  every_route f 1;
  Alcotest.(check int) "every route admits what the guard allows" 6 (total f);
  Alcotest.(check int) "no more refusals" 6 (List.length (refusals f))

let a_root_guard_reads_the_state_when_the_action_runs () =
  (* Blocked after the Event was sent, before its Action ran: refused. *)
  let f = fixture (blocking []) in
  bump f 5;
  ignore (dispatch f f.app_mount (Block 5));
  drain f;
  Alcotest.(check int) "total" 0 (total f);
  check_refusals "refused" [ root_guard "/mid/leaf" "queued" ] f;
  (* Unblocked in between: admitted. *)
  let f = fixture (blocking [ 5 ]) in
  bump f 5;
  ignore (dispatch f f.app_mount (Unblock 5));
  drain f;
  Alcotest.(check int) "total" 5 (total f);
  check_refusals "admitted" [] f;
  (* The leaf's own State, as it stands: the limit counts what was added. *)
  let f = fixture { (blocking []) with limit = 10 } in
  Alcotest.check outcome "6" Executed (to_leaf f (Add 6));
  Alcotest.check outcome "6 more" Disabled (to_leaf f (Add 6));
  Alcotest.check outcome "4 more" Executed (to_leaf f (Add 4));
  Alcotest.(check int) "total" 10 (total f)

let two_mounts_of_one_component_have_their_own_root_guards () =
  let f = fixture { (blocking [ 5 ]) with other_blocked = [ 7 ] } in
  List.iter
    (fun n ->
      ignore (dispatch f f.leaf_mount (Add n));
      ignore (dispatch f f.other_leaf_mount (Add n)))
    [ 5; 7 ];
  Alcotest.(check (pair int int)) "totals" (7, 5) (total f, other_total f);
  check_refusals "refusals"
    [ root_guard "/mid/leaf" "root"; root_guard "/other/leaf" "root" ]
    f

let an_inline_call_below_the_root_does_not_run_the_root_guard_and_says_so () =
  let f = fixture { (blocking []) with other_blocked = [ 7 ] } in
  let forward n = ignore (dispatch f f.other_mount (Forward (Add n))) in
  (* /other has no root guard for what it forwards: the call is the
     caller's, and the leaf's root guard does not run. *)
  forward 7;
  Alcotest.(check int) "added" 7 (other_total f);
  (match Observe.Trace_log.inline f.log with
  | (i : Observe.Inline_entry.t) :: _ ->
      Alcotest.(check (pair string bool))
        "path, root_guard_skipped" ("/other/leaf", true)
        (i.path, i.root_guard_skipped)
  | [] -> Alcotest.fail "an inline call");
  (* The slot guard and the leaf's own check still run inline. *)
  forward 13;
  forward 100;
  Alcotest.(check int) "nothing more" 7 (other_total f);
  let inline_refusal by = ("/other/leaf", "leaf.add", by, "inline") in
  check_refusals "refusals" [ inline_refusal "guard"; inline_refusal "component" ] f;
  Alcotest.(check (list string))
    "outcomes"
    [ "reduced"; "refused"; "refused" ]
    (List.map
       (function Reduced -> "reduced" | Refused -> "refused")
       (Store.state f.store).other.outcomes)

let an_inline_call_from_the_root_runs_the_root_guard () =
  let f = fixture (blocking [ 5 ]) in
  let call n = ignore (dispatch f f.app_mount (Mid (Forward (Add n)))) in
  (* The root holds the whole State, so its inline call into /mid is
     checked by the root guard there. *)
  call 5;
  check_refusals "refusals" [ ("/mid", "mid.forward", "root-guard", "inline") ] f;
  call 2;
  Alcotest.(check int) "total" 2 (total f);
  (* Told as run (/mid) and as skipped by /mid's own inline call
     (/mid/leaf). *)
  Alcotest.(check (list (pair string bool)))
    "inline calls"
    [ ("/mid", false); ("/mid/leaf", true) ]
    (List.map
       (fun (i : Observe.Inline_entry.t) -> (i.path, i.root_guard_skipped))
       (Observe.Trace_log.inline f.log))

let the_root_guard_on_the_root_s_inline_call_reads_its_changes_so_far () =
  (* The root blocks 4 and then calls /mid inline: the root guard there
     reads the State the root's reducer has made, not the one it began
     with. *)
  let f = fixture (blocking []) in
  ignore (dispatch f f.app_mount (Block_then_forward 4));
  Alcotest.(check int) "total" 0 (total f);
  check_refusals "refusals" [ ("/mid", "mid.forward", "root-guard", "inline") ] f

let the_root_s_inline_call_checks_in_the_queued_order () =
  (* 131 is blocked (the root guard) and ends in 31 (App's slot guard on
     /mid); 1031 ends in 31 and is 1000 or more (Mid's own check); 1000 is
     only the latter. /spare is absent, and its root guard refuses all. *)
  let f = fixture (blocking [ 131 ]) in
  List.iter
    (fun call -> ignore (dispatch f f.app_mount call))
    [
      Mid (Forward (Add 131));
      Mid (Forward (Add 1031));
      Mid (Forward (Add 1000));
      Spare (Forward (Add 5));
    ];
  let inline path by = (path, "mid.forward", by, "inline") in
  check_refusals "refusals"
    [
      inline "/mid" "root-guard";
      inline "/mid" "guard";
      inline "/mid" "component";
      inline "/spare" "absent";
    ]
    f

let a_root_guard_s_bug_report_is_placed_at_the_root () =
  let f = fixture (blocking []) in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer f.store (Observe.Bug_log.observer bugs);
  Alcotest.check outcome "refused" Disabled (to_leaf f (Add 777));
  match Observe.Bug_log.reports bugs with
  | (report : Observe.Bug_report.t) :: _ ->
      let place = Option.get report.place in
      Alcotest.(check (pair bool string))
        "at admission, at the root" (true, "/")
        (place.phase = Observe.Admission, place.path)
  | [] -> Alcotest.fail "a bug report"

let a_forwarded_action_is_checked_where_it_entered () =
  (* The root guard on /mid checks the wrapper, on every route into /mid,
     so the addition never reaches the leaf. *)
  let f = fixture (blocking [ 5 ]) in
  Alcotest.check outcome "blocked" Disabled (to_mid f (Forward (Add 5)));
  Alcotest.(check int) "total" 0 (total f);
  check_refusals "refusals" [ ("/mid", "mid.forward", "root-guard", "root") ] f;
  Alcotest.check outcome "admitted" Executed (to_mid f (Forward (Add 6)));
  Alcotest.(check int) "total" 6 (total f)

let the_checks_run_in_order_and_the_first_refusal_is_told () =
  let refused f action =
    let before = List.length (refusals f) in
    Alcotest.check outcome "refused" Disabled (to_leaf f action);
    let _, _, by, _ = List.nth (refusals f) before in
    by
  in
  (* 113 ends in 13 (the slot guard) and is 100 or more (the leaf's own
     check); 150 is too; 13 is blocked by the root guard as well. *)
  let f = fixture (blocking [ 13; 150 ]) in
  Alcotest.(check string) "113" "guard" (refused f (Add 113));
  Alcotest.(check string) "150" "root-guard" (refused f (Add 150));
  Alcotest.(check string) "13" "root-guard" (refused f (Add 13));
  (* With the leaf gone, every check would refuse 13; availability comes
     first, on the queued path and inline. *)
  ignore (to_mid f Drop_leaf);
  Alcotest.(check string) "13, absent" "absent" (refused f (Add 13));
  let before = List.length (refusals f) in
  ignore (to_mid f (Send (Add 113)));
  ignore (dispatch f f.other_mount Drop_leaf);
  ignore (dispatch f f.other_mount (Forward (Add 113)));
  let after =
    List.filteri (fun i _ -> i >= before) (refusals f)
    |> List.map (fun (_, _, by, sent) -> (by, sent))
  in
  Alcotest.(check (list (pair string string)))
    "absent first"
    [ ("absent", "queued"); ("absent", "inline") ]
    after

let a_panicking_root_guard_faults_the_store () =
  let f = fixture (blocking []) in
  match Store.dispatch_to f.store f.leaf_mount (Add 666) with
  | Error (Faulted m) when String.equal m "a root guard that panics" -> ()
  | Error (Faulted m) -> Alcotest.failf "another fault: %s" m
  | _ -> Alcotest.fail "the Store faults"

let freeze_reports_misplaced_root_guards () =
  let nowhere = Child.make "nowhere" leaf in
  let not_a_tip = Child.make "leaf" tip in
  let never _ _ _ _ = false in
  let reg = Registry.create () in
  Registry.root reg app;
  Registry.admit reg (Path.child (Path.child (Path.root app) mid_slot) nowhere) never;
  Registry.admit reg (Path.child (Path.root mid) leaf_slot) never;
  Registry.admit reg (Path.child (Path.child (Path.root app) mid_slot) not_a_tip) never;
  Registry.admit reg (Path.root app) never;
  (* A child of the root is fine. *)
  Registry.admit reg (mid_path ()) never;
  Registry.admit reg (mid_leaf ()) never;
  Registry.admit reg (mid_leaf ()) never;
  match Registry.freeze reg with
  | Ok _ -> Alcotest.fail "the registration is refused"
  | Error problems ->
      let problem =
        Alcotest.testable (fun f p -> Format.pp_print_string f (problem_message p)) ( = )
      in
      Alcotest.(check (list problem))
        "problems"
        [
          Root_guard_no_mount "/mid/nowhere";
          Root_guard_other_component "/";
          Root_guard_other_component "/mid/leaf";
          Root_guard_on_root;
          Root_guard_twice "/mid/leaf";
        ]
        problems

let a_path_from_another_root_does_not_resolve () =
  (* Through a slot of this Protocol's tree, /mid, but from another root:
     refused at /, as a root guard's path is at freeze. *)
  let f = fixture (blocking []) in
  let from_tip = Path.child (Path.root tip) mid_slot in
  let at_root = function Error (Other_component "/") -> true | _ -> false in
  Alcotest.(check bool)
    "resolve" true
    (at_root (Protocol.resolve (Store.protocol f.store) from_tip));
  Alcotest.(check bool) "mount" true (at_root (Store.mount f.store from_tip));
  Alcotest.(check bool)
    "from this root" true
    (Result.is_ok (Store.mount f.store (mid_path ())))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "root guards"
    [
      ( "root guards",
        [
          t "a root guard checks every Action that enters its mount"
            a_root_guard_checks_every_action_that_enters_its_mount;
          t "a root guard reads the State when the Action runs"
            a_root_guard_reads_the_state_when_the_action_runs;
          t "two mounts of one component have their own root guards"
            two_mounts_of_one_component_have_their_own_root_guards;
          t "an inline call below the root does not run the root guard and says so"
            an_inline_call_below_the_root_does_not_run_the_root_guard_and_says_so;
          t "an inline call from the root runs the root guard"
            an_inline_call_from_the_root_runs_the_root_guard;
          t "the root guard on the root's inline call reads its changes so far"
            the_root_guard_on_the_root_s_inline_call_reads_its_changes_so_far;
          t "the root's inline call checks in the queued order"
            the_root_s_inline_call_checks_in_the_queued_order;
          t "a root guard's bug report is placed at the root"
            a_root_guard_s_bug_report_is_placed_at_the_root;
          t "a forwarded Action is checked where it entered"
            a_forwarded_action_is_checked_where_it_entered;
          t "the checks run in order and the first refusal is told"
            the_checks_run_in_order_and_the_first_refusal_is_told;
          t "a panicking root guard faults the Store"
            a_panicking_root_guard_faults_the_store;
          t "freeze reports misplaced root guards" freeze_reports_misplaced_root_guards;
          t "a path from another root does not resolve"
            a_path_from_another_root_does_not_resolve;
        ] );
    ]
