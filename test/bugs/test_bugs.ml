(* Bug reports ([Bug.condition]): the caller's fallback runs, the report
   names its place, and observers get it. Ported from the Rust port's
   tests/bugs.rs. *)

open Causet
open Bugs_common

(* ------------------------------------------------------------------------ *)
(* A parent and a child, with every place a report can come from. *)

type kid_state = { pings : int }

type kid_action =
  | Ping of { report : bool }
  | Refuse  (** Its enabling check reports and refuses it. *)
  | Guarded  (** The parent's guard reports and refuses it. *)
  | Shout  (** Reported as an Output the parent's lowerer reports about. *)
  | Crash_report

type kid_output = Shouted

let kid_actions =
  Message.make ~name:"kid.action"
    ~kind:(function
      | Ping _ -> "kid.ping"
      | Refuse -> "kid.refuse"
      | Guarded -> "kid.guarded"
      | Shout -> "kid.shout"
      | Crash_report -> "kid.crash_report")
    ()

let kid_outputs = Message.make ~name:"kid.output" ~kind:(fun Shouted -> "kid.shouted") ()

let kid =
  Component.make ~name:"kid" ~actions:kid_actions ~effects:Message.never
    ~outputs:kid_outputs
    ~is_enabled:(fun _ action _ ->
      match action with
      | Refuse ->
          Bug.condition __POS__ "kid refuses";
          false
      | _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Ping { report } ->
          (if report then
             let pings = (Ctx.state ctx).pings in
             Bug.condition ~fields:[ ("pings", string_of_int pings) ] __POS__ "kid ping");
          Ctx.update ctx (fun s -> { pings = s.pings + 1 })
      | Crash_report ->
          Bug.condition __POS__ "kid before panic";
          failwith "kid panic"
      | Shout -> Ctx.emit_output ctx Shouted
      | Refuse | Guarded -> failwith "unreachable: refused")
    ()

type tree_state = { kid : kid_state; notes : int }
type kid_action_tag = Tag_refuse | Tag_guarded | Tag_shout

type tree_action =
  | Inline of { before : bool; kid : bool; after : bool }
  | Queue of kid_action_tag
  | Fx
  | Noted
  | Crash
  | Admission_bug of { refuse : bool }
  | Fx_admission of { refuse : bool }
  | Inline_crash

let tree_actions =
  Message.make ~name:"tree.action"
    ~kind:(function
      | Inline _ -> "tree.inline"
      | Queue _ -> "tree.queue"
      | Fx -> "tree.fx"
      | Noted -> "tree.noted"
      | Crash -> "tree.crash"
      | Admission_bug _ -> "tree.admission_bug"
      | Fx_admission _ -> "tree.fx_admission"
      | Inline_crash -> "tree.inline_crash")
    ()

type work = { mode : int }

let works = Message.make ~name:"tree.effect" ~kind:(fun _ -> "tree.work") ()

type poked = Poked

let pokes : poked Message.t = Message.single "tree.poked"
let kid_slot = Child.make "kid" kid

let guard _ action _ =
  match action with
  | Guarded ->
      Bug.condition __POS__ "guard refuses";
      false
  | _ -> true

let lower _ Shouted port =
  Bug.condition __POS__ "lowered";
  Port.emit_action port Noted

let tree =
  Component.make ~name:"tree" ~actions:tree_actions ~effects:works ~outputs:Message.never
    ~is_enabled:(fun _ action _ ->
      match action with
      | Admission_bug { refuse } ->
          Bug.condition __POS__ "sync admission";
          not refuse
      | _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Inline { before; kid; after } ->
          if before then Bug.condition __POS__ "parent before";
          ignore (Ctx.reduce_child ctx kid_slot (Ping { report = kid }));
          if after then Bug.condition __POS__ "parent after"
      | Queue tag ->
          let action =
            match tag with
            | Tag_refuse -> Refuse
            | Tag_guarded -> Guarded
            | Tag_shout -> Shout
          in
          Ctx.to_child ctx kid_slot action
      | Fx -> Ctx.emit_effect ctx { mode = 0 }
      | Fx_admission { refuse } ->
          Ctx.emit_effect ctx { mode = (if refuse then 2 else 1) }
      | Inline_crash -> ignore (Ctx.reduce_child ctx kid_slot Crash_report)
      | Admission_bug _ | Noted ->
          Ctx.update ctx (fun s -> { s with notes = s.notes + 1 })
      | Crash ->
          Bug.condition __POS__ "about to crash";
          failwith "crashed")
    ~handle:(fun ctx work _ ->
      if work.mode = 1 || work.mode = 2 then
        ignore (Effect_ctx.dispatch ctx (Admission_bug { refuse = work.mode = 2 }))
      else (
        Bug.condition __POS__ "handler before";
        ignore (Effect_ctx.dispatch ctx Noted);
        Bug.condition __POS__ "handler after"))
    ~register:(fun r ->
      Registrar.event r pokes (fun Poked port ->
          Bug.condition __POS__ "adapter";
          Port.emit_action port Noted);
      Registrar.child r kid_slot
        ~lens:(Lens.field ~get:(fun s -> s.kid) ~set:(fun s kid -> { s with kid }))
        ~guard ~on_output:lower ())
    ()

(* Everything observers see, in order: starts and reports. *)
let order_observer order =
  {
    Observe.none with
    action_start = (fun e _ -> order := !order @ [ "start " ^ e.kind ]);
    effect_start = (fun e _ -> order := !order @ [ "start " ^ e.kind ]);
    bug = (fun r -> order := !order @ [ "bug " ^ r.message ]);
  }

type fixture = {
  store : (tree_state, unit) Store.t;
  sink : Sink.t;
  tree_m : (tree_state, unit, tree_action, work, never) mount;
  protocol : (tree_state, unit) Protocol.t;
  log : Observe.Bug_log.log;
}

let protocol () =
  let reg = Registry.create () in
  Registry.root reg tree;
  match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze"

let fixture_with_clock clock =
  let protocol = protocol () in
  let sink, inbox = Event_bus.create protocol ~worker_capacity:16 in
  let store =
    Store.create_exn ~clock protocol
      ~state:{ kid = { pings = 0 }; notes = 0 }
      ~services:() inbox
  in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  let tree_m = Protocol.resolve_exn protocol (Path.root tree) in
  { store; sink; tree_m; protocol; log }

let fixture () = fixture_with_clock (Clock.Manual.clock (Clock.Manual.create 1000))
let reports f = Observe.Bug_log.reports f.log
let summaries f = List.map summary (reports f)

let messages log =
  List.map (fun (r : Observe.Bug_report.t) -> r.message) (Observe.Bug_log.reports log)

let kid_mount f = Protocol.resolve_exn f.protocol (Path.child (Path.root tree) kid_slot)
let strings = Alcotest.(list string)

let one what = function
  | [ r ] -> r
  | rs -> Alcotest.failf "%s: expected one report, got %d" what (List.length rs)

(* ------------------------------------------------------------------------ *)

let a_report_is_placed_and_the_fallback_runs () =
  let f = fixture () in
  check_outcome "outcome" (Ok Executed)
    (Store.dispatch_to f.store f.tree_m
       (Inline { before = false; kid = true; after = false }));
  Alcotest.(check int) "the code after the report ran" 1 (Store.state f.store).kid.pings;
  let report = one "reports" (reports f) in
  Alcotest.(check string) "summary" "kid ping | Reducer | 1 | /kid" (summary report);
  Alcotest.(check (list (pair string string))) "fields" [ ("pings", "0") ] report.fields;
  Alcotest.(check bool)
    ("file " ^ report.file) true
    (ends_with ~suffix:"test_bugs.ml" report.file);
  let place = Option.get report.place in
  Alcotest.(check int) "sequence" 0 place.sequence;
  Alcotest.(check int64) "component" (Protocol.instance_of (kid_mount f)) place.component

let reports_name_the_component_whose_code_made_them () =
  let f = fixture () in
  let run a = ignore (Store.dispatch_exn f.store f.tree_m a) in
  run (Inline { before = true; kid = true; after = true });
  run (Queue Tag_guarded);
  run (Queue Tag_refuse);
  run (Queue Tag_shout);
  Alcotest.check strings "places"
    [
      (* One execution: the parent, the child it called inline, the parent. *)
      "parent before | Reducer | 1 | /";
      "kid ping | Reducer | 1 | /kid";
      "parent after | Reducer | 1 | /";
      (* Queued Actions refused at admission: the guard is the parent's code,
         the enabling check the child's; both name the execution that queued
         the Action. *)
      "guard refuses | Admission | 2 | /";
      "kid refuses | Admission | 3 | /kid";
      (* The child's Output, lowered by the parent, in the child's execution. *)
      "lowered | Reducer | 5 | /";
    ]
    (summaries f);
  Alcotest.(check (list int))
    "sequences" [ 0; 1; 2; 3; 4; 5 ]
    (List.map
       (fun (r : Observe.Bug_report.t) -> (Option.get r.place).sequence)
       (reports f));
  Alcotest.(check int) "notes" 1 (Store.state f.store).notes

let a_root_action_refused_at_admission_names_no_execution () =
  let f = fixture () in
  check_outcome "outcome" (Ok Disabled) (Store.dispatch_to f.store (kid_mount f) Refuse);
  Alcotest.(check string)
    "summary" "kid refuses | Admission | None | /kid"
    (summary (one "reports" (reports f)))

let a_handler_report_keeps_its_order_around_a_synchronous_dispatch () =
  let f = fixture () in
  let order = ref [] in
  Store.add_observer f.store (order_observer order);
  ignore (Store.dispatch_exn f.store f.tree_m Fx);
  Alcotest.check strings "order"
    [
      "start tree.fx";
      "start tree.work";
      "bug handler before";
      "start tree.noted";
      "bug handler after";
    ]
    !order;
  match summaries f with
  | first :: second :: _ ->
      Alcotest.(check string) "first" "handler before | Handler | 2 | /" first;
      Alcotest.(check string) "second" "handler after | Handler | 2 | /" second
  | _ -> Alcotest.fail "two reports"

let an_adapter_report_is_placed_in_the_event () =
  let f = fixture () in
  (match Sink.try_send_to f.sink f.tree_m pokes Poked with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "send");
  ignore (Store.drain_events f.store 4);
  Alcotest.(check string)
    "summary" "adapter | Adapter | 1 | /"
    (summary (one "reports" (reports f)))

let reports_made_before_a_fault_are_kept () =
  let f = fixture () in
  Alcotest.(check bool)
    "faulted" true
    (Result.is_error (Store.dispatch_to f.store f.tree_m Crash));
  Alcotest.(check string)
    "summary" "about to crash | Reducer | 1 | /"
    (summary (one "reports" (reports f)))

let an_observer_report_is_placed () =
  let nosy =
    {
      Observe.none with
      action_start =
        (fun e _ -> if e.kind = "tree.noted" then Bug.condition __POS__ "observer");
    }
  in
  let f = fixture () in
  Store.add_observer f.store nosy;
  ignore (Store.dispatch_exn f.store f.tree_m Noted);
  Alcotest.(check string)
    "summary" "observer | Observer(Start) | 1 | /"
    (summary (one "reports" (reports f)))

let the_harness_keeps_reports_without_a_place () =
  let h = Harness.create () in
  let state = { pings = 0 } in
  let state, _ = Harness.reduce h kid state (Ping { report = true }) in
  Alcotest.(check bool) "refused" false (Harness.is_enabled h kid state Refuse);
  Alcotest.(check (list (pair string bool)))
    "reports"
    [ ("kid ping", true); ("kid refuses", true) ]
    (List.map
       (fun (r : Observe.Bug_report.t) -> (r.message, r.place = None))
       (Harness.bug_reports h))

let outside_a_store_a_report_goes_to_the_hook () =
  let lock = Mutex.create () in
  let seen = ref [] in
  Bug.set_hook (fun r ->
      if r.place = None && String.starts_with ~prefix:"outside" r.message then
        Mutex.protect lock (fun () -> seen := r.message :: !seen));
  let helper n =
    if n = 0 then (
      Bug.condition __POS__ "outside, from a helper";
      1)
    else n
  in
  Alcotest.(check int) "fallback" 1 (helper 0);
  Thread.join
    (Thread.create (fun () -> Bug.condition __POS__ "outside, from a Service thread") ());
  Bug.set_hook Bug.default_hook;
  Alcotest.check strings "seen"
    [ "outside, from a Service thread"; "outside, from a helper" ]
    (List.sort compare !seen)

let a_store_run_inside_another_keeps_its_reports_apart () =
  (* The outer Store's handler runs a whole inner Store, which reports too. *)
  let inner_reports = ref [] in
  let gos : unit Message.t =
    Message.make ~name:"outer.action" ~kind:(fun () -> "outer.go") ()
  in
  let nests : unit Message.t =
    Message.make ~name:"outer.effect" ~kind:(fun () -> "outer.nest") ()
  in
  let outer =
    Component.make ~name:"outer" ~actions:gos ~effects:nests ~outputs:Message.never
      ~reduce:(fun ctx () _ -> Ctx.emit_effect ctx ())
      ~handle:(fun _ () _ ->
        Bug.condition __POS__ "outer before";
        let inner = fixture () in
        ignore
          (Store.dispatch_exn inner.store inner.tree_m
             (Inline { before = true; kid = false; after = false }));
        inner_reports := messages inner.log;
        Bug.condition __POS__ "outer after")
      ()
  in
  let reg = Registry.create () in
  Registry.root reg outer;
  let protocol =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze"
  in
  let _sink, inbox = Event_bus.create protocol ~worker_capacity:4 in
  let store = Store.create_exn protocol ~state:() ~services:() inbox in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  ignore (Store.dispatch_exn store (Protocol.resolve_exn protocol (Path.root outer)) ());
  Alcotest.check strings "inner" [ "parent before" ] !inner_reports;
  Alcotest.check strings "outer" [ "outer before"; "outer after" ] (messages log)

(* The OCaml Bug_log has no [assert_none] / [assert_messages]: the test reads
   the reports it would have asserted on. *)
let a_test_checks_reports_after_the_run () =
  let f = fixture () in
  ignore (Store.dispatch_exn f.store f.tree_m Noted);
  Alcotest.check strings "none" [] (messages f.log);
  ignore
    (Store.dispatch_exn f.store f.tree_m
       (Inline { before = false; kid = true; after = false }));
  Alcotest.check strings "messages" [ "kid ping" ] (messages f.log)

let a_child_that_reports_then_panics_keeps_its_report () =
  let f = fixture () in
  Alcotest.(check bool)
    "faulted" true
    (Result.is_error (Store.dispatch_to f.store f.tree_m Inline_crash));
  let all = reports f in
  Alcotest.(check int) "one" 1 (List.length all);
  Alcotest.(check string) "path" "/kid" (Option.get (List.hd all).place).path

let an_observer_that_reports_then_panics_keeps_the_observed_mount () =
  let crashing =
    {
      Observe.none with
      action_start =
        (fun e _ ->
          if e.path = "/kid" then (
            Bug.condition __POS__ "observer before panic";
            failwith "observer panic"));
    }
  in
  let f = fixture () in
  Store.add_observer f.store crashing;
  Alcotest.(check bool)
    "faulted" true
    (Result.is_error (Store.dispatch_to f.store f.tree_m (Queue Tag_shout)));
  let place = Option.get (one "reports" (reports f)).place in
  Alcotest.(check (pair string string))
    "place" ("Observer(Start)", "/kid")
    (phase_string place.phase, place.path)

let an_effect_childs_admission_report_names_the_effect () =
  let f = fixture () in
  ignore (Store.dispatch_exn f.store f.tree_m (Fx_admission { refuse = false }));
  ignore (Store.dispatch_exn f.store f.tree_m (Fx_admission { refuse = true }));
  Alcotest.check strings "places"
    [
      (* The Effects were executions 2 and 5: the admission reports name them. *)
      "sync admission | Admission | 2 | /";
      "sync admission | Admission | 5 | /";
    ]
    (summaries f);
  Alcotest.(check int) "only the admitted child ran" 1 (Store.state f.store).notes

(* Reports while the synchronous child starts: its fourth reading follows
   Store construction, the root Action and the Effect. *)
let reporting_clock () =
  let readings = ref 0 in
  {
    Clock.now =
      (fun () ->
        incr readings;
        if !readings = 4 then Bug.condition __POS__ "clock at child start";
        1000);
  }

let a_clock_report_is_placed_in_the_clock_phase () =
  let f = fixture_with_clock (reporting_clock ()) in
  ignore (Store.dispatch_exn f.store f.tree_m (Fx_admission { refuse = false }));
  Alcotest.check strings "places"
    [ "sync admission | Admission | 2 | /"; "clock at child start | Clock | None | /" ]
    (summaries f)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "bugs"
    [
      ( "bugs",
        [
          t "a report is placed and the fallback runs"
            a_report_is_placed_and_the_fallback_runs;
          t "reports name the component whose code made them"
            reports_name_the_component_whose_code_made_them;
          t "a root action refused at admission names no execution"
            a_root_action_refused_at_admission_names_no_execution;
          t "a handler report keeps its order around a synchronous dispatch"
            a_handler_report_keeps_its_order_around_a_synchronous_dispatch;
          t "an adapter report is placed in the event"
            an_adapter_report_is_placed_in_the_event;
          t "reports made before a fault are kept" reports_made_before_a_fault_are_kept;
          t "an observer report is placed" an_observer_report_is_placed;
          t "the harness keeps reports without a place"
            the_harness_keeps_reports_without_a_place;
          t "outside a store a report goes to the hook"
            outside_a_store_a_report_goes_to_the_hook;
          t "a store run inside another keeps its reports apart"
            a_store_run_inside_another_keeps_its_reports_apart;
          t "a test checks reports after the run" a_test_checks_reports_after_the_run;
          t "a child that reports then panics keeps its report"
            a_child_that_reports_then_panics_keeps_its_report;
          t "an observer that reports then panics keeps the observed mount"
            an_observer_that_reports_then_panics_keeps_the_observed_mount;
          t "an effect child's admission report names the effect"
            an_effect_childs_admission_report_names_the_effect;
          t "a clock report is placed in the clock phase"
            a_clock_report_is_placed_in_the_clock_phase;
        ] );
    ]
