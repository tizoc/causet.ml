(* Port of causet-test's tests/invariant_edges.rs: an Action's kind is read
   inside the Store's fault boundary, and only once it is admitted or to
   tell an observer of its refusal; a checker's own bug reports reach
   observers; a refused Action settles no group; and a failed publication
   leaves no member behind. *)

open Causet
open Causet_test

type input =
  | Inc
  | Refused
  | Panic_kind
  | Refused_kind
      (** Refused, and its kind raises: a refused Action's kind is never read. *)

let inputs =
  Message.make ~name:"review.invariant.action"
    ~kind:(function
      | Inc -> "review.inc"
      | Refused -> "review.refused"
      | Panic_kind | Refused_kind -> failwith "the kind callback failed")
    ()

let counter =
  Component.make ~name:"counter" ~actions:inputs ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ ->
      match action with Refused | Refused_kind -> false | _ -> true)
    ~reduce:(fun ctx _ _ -> Ctx.update ctx (( + ) 1))
    ()

let store () =
  let reg = Registry.create () in
  Registry.root reg counter;
  let proto = match Registry.freeze reg with Ok p -> p | Error _ -> assert false in
  let _sink, inbox = Event_bus.create proto ~worker_capacity:8 in
  let mount = Protocol.resolve_exn proto (Path.root counter) in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 0))
      proto ~state:0 ~services:() inbox
  in
  (store, mount)

let is_faulted = function Ok (Error (Faulted _)) -> true | _ -> false

(* The Rust Store reads every Action's kind, so there the ordinary Store
   faults too; this one reads it only for an invariant or an observer, and
   runs the Action. *)
let adding_an_invariant_does_not_let_kind_panics_escape_the_store () =
  let ordinary, mount = store () in
  (* An observer of reports alone is told of no execution: no kind is read. *)
  Store.add_observer ordinary (Observe.Bug_log.observer (Observe.Bug_log.create ()));
  Alcotest.(check bool)
    "ordinary: executed" true
    (Store.dispatch_to ordinary mount Panic_kind = Ok Executed);
  let checked, mount = store () in
  Store.add_invariant checked "always valid" (fun _ _ -> Ok ());
  let checked_result =
    match Store.dispatch_to checked mount Panic_kind with
    | r -> Ok r
    | exception e -> Error e
  in
  Alcotest.(check bool)
    (Printf.sprintf "the kind panic escaped; Store fault: %s"
       (Option.value (Store.fault checked) ~default:"none"))
    true (is_faulted checked_result)

let invariants_do_not_inspect_the_kind_of_a_refused_action () =
  let ordinary, mount = store () in
  Alcotest.(check bool)
    "ordinary: disabled" true
    (Store.dispatch_to ordinary mount Refused_kind = Ok Disabled);
  let checked, mount = store () in
  Store.add_invariant checked "always valid" (fun _ _ -> Ok ());
  Alcotest.(check bool)
    "checked: disabled" true
    (Store.dispatch_to checked mount Refused_kind = Ok Disabled);
  Alcotest.(check bool) "no fault" true (Store.fault checked = None)

(* A refused Action's kind is read only to tell an observer of refusals: one
   that observes faults alone reads none. Once an observer of refusals is
   installed, reading the kind is part of telling it, and a kind that raises
   faults the Store as that observer boundary's fault. *)
let a_refused_actions_kind_is_read_only_to_tell_an_observer () =
  let faults = ref [] in
  let fault (f : Observe.Fault_info.t) = faults := !faults @ [ f.phase ] in
  let unconcerned, mount = store () in
  Store.add_observer unconcerned { Observe.none with fault };
  Alcotest.(check bool)
    "unconcerned: disabled" true
    (Store.dispatch_to unconcerned mount Refused_kind = Ok Disabled);
  Alcotest.(check int) "no fault: the kind was not read" 0 (List.length !faults);
  let observed, mount = store () in
  Store.add_observer observed
    { Observe.none with fault; action_refused = (fun _ _ -> ()) };
  Alcotest.(check bool)
    "an error" true
    (Result.is_error (Store.dispatch_to observed mount Refused_kind));
  Alcotest.(check bool) "phases" true (!faults = [ Observe.Observer Observe.Refused ])

let group_checks_do_not_run_for_a_refused_action () =
  let group = Group.create "pair" in
  let node =
    Node.publish
      (Node.create ~name:"counter" counter ~state:0 ~services:() ~session:1
         (Clock.Manual.create 0))
      group Fun.id
  in
  let calls = ref 0 in
  Group.check group "settled roots" (fun _ step ->
      (match step with Settled _ -> incr calls | _ -> ());
      Ok ());
  let mount = Node.mount node (Path.root counter) in
  Alcotest.(check bool) "disabled" true (Node.dispatch node mount Refused = Disabled);
  Alcotest.(check int) "no root execution settled" 0 !calls

let a_bug_report_inside_a_checker_reaches_store_observers () =
  let store, mount = store () in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  Store.add_invariant store "diagnostic" (fun state _ ->
      if state = 1 then Bug.condition __POS__ "diagnostic from inside a checker";
      Ok ());
  ignore (Store.dispatch_exn store mount Inc);
  let reports = Observe.Bug_log.reports log in
  Alcotest.(check int)
    "the callback's report was silently discarded" 1 (List.length reports);
  let r = List.hd reports in
  (* Made while the checker ran, so placed under its phase. *)
  Alcotest.(check bool)
    "phase" true
    ((Option.get r.place).phase = Observe.Invariant "diagnostic");
  Alcotest.(check string) "message" "diagnostic from inside a checker" r.message

let failed_publication_does_not_leave_a_ghost_member () =
  let group = Group.create "pair" in
  Group.check group "positive" (fun views _ ->
      if Group.view views "counter" = Some 0 then Error "zero" else Ok ());
  let failed =
    match
      Node.publish
        (Node.create ~name:"counter" counter ~state:0 ~services:() ~session:1
           (Clock.Manual.create 0))
        group Fun.id
    with
    | _ -> false
    | exception _ -> true
  in
  Alcotest.(check bool) "the first publication failed" true failed;
  let replacement =
    match
      Node.publish
        (Node.create ~name:"counter" counter ~state:1 ~services:() ~session:2
           (Clock.Manual.create 0))
        group Fun.id
    with
    | _ -> Ok ()
    | exception e -> Error (Harness_util.message_of e)
  in
  Alcotest.(check (result unit string))
    "a dropped failed publication still occupies the name" (Ok ()) replacement

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "invariant_edges"
    [
      ( "edges",
        [
          t "adding an invariant does not let kind panics escape the store"
            adding_an_invariant_does_not_let_kind_panics_escape_the_store;
          t "invariants do not inspect the kind of a refused action"
            invariants_do_not_inspect_the_kind_of_a_refused_action;
          t "a refused action's kind is read only to tell an observer of refusals"
            a_refused_actions_kind_is_read_only_to_tell_an_observer;
          t "group checks do not run for a refused action"
            group_checks_do_not_run_for_a_refused_action;
          t "a bug report inside a checker reaches store observers"
            a_bug_report_inside_a_checker_reaches_store_observers;
          t "failed publication does not leave a ghost member"
            failed_publication_does_not_leave_a_ghost_member;
        ] );
    ]
