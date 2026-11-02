(* Invariants on the Store: when a checker runs, what it is told, and how a
   violation and a checker's own exception are reported without stopping the
   Store. Ported from the Rust port's tests/invariants.rs. *)

open Causet
open Bugs_common

type tally_action = Add of int  (** Refused when 0. *) | Boom

let tally_actions =
  Message.make ~name:"tally.action"
    ~kind:(function Add _ -> "tally.add" | Boom -> "tally.boom")
    ()

type tally_ping = Tally_ping of int

let tally_pings : tally_ping Message.t = Message.single "tally.ping"

let tally =
  Component.make ~name:"tally" ~actions:tally_actions ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> action <> Add 0)
    ~reduce:(fun ctx action _ ->
      match action with
      | Add n -> Ctx.update ctx (( + ) n)
      | Boom ->
          Ctx.set_state ctx 999 (* half changed, then the fault *);
          failwith "boom")
    ~register:(fun r ->
      Registrar.event r tally_pings (fun (Tally_ping n) port ->
          Port.emit_action port (Add n)))
    ()

type fixture = {
  store : (int, unit) Store.t;
  sink : Sink.t;
  tally_m : (int, unit, tally_action, never, never) mount;
  log : Observe.Bug_log.log;
}

let fixture () =
  let reg = Registry.create () in
  Registry.root reg tally;
  let protocol =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze"
  in
  let sink, inbox = Event_bus.create protocol ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      protocol ~state:0 ~services:() inbox
  in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  { store; sink; tally_m = Protocol.resolve_exn protocol (Path.root tally); log }

(* Every step a checker saw, in words. *)
let recording seen state step =
  let step =
    match step with
    | Added -> "added"
    | Restored -> "restored"
    | Settled_step { category; kind; path; _ } ->
        Printf.sprintf "%s %s at %s" (category_string category) kind path
  in
  seen := !seen @ [ Printf.sprintf "%s: %d" step state ];
  Ok ()

let reports f = Observe.Bug_log.reports f.log
let strings = Alcotest.(list string)

let send f n =
  match Sink.try_send_to f.sink f.tally_m tally_pings (Tally_ping n) with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "send"

let drained what expected got =
  Alcotest.(check string)
    what
    (match expected with Ok n -> "Ok " ^ string_of_int n | Error _ -> "Error")
    (match got with
    | Ok n -> "Ok " ^ string_of_int n
    | Error e -> outcome_string (Error e))

let a_checker_runs_when_added_and_after_each_settled_root_input_only () =
  let f = fixture () in
  let seen = ref [] in
  Store.add_invariant f.store "records" (recording seen);
  ignore (Store.dispatch_exn f.store f.tally_m (Add 2));
  check_outcome "refused" (Ok Disabled) (Store.dispatch_to f.store f.tally_m (Add 0));
  drained "an empty drain" (Ok 0) (Store.drain_events f.store 8);
  send f 3;
  send f 4;
  drained "one check per Event" (Ok 2) (Store.drain_events f.store 8);
  Alcotest.check strings "seen"
    [
      "added: 0";
      "Action tally.add at /: 2";
      "Event tally.ping at /: 5";
      "Event tally.ping at /: 9";
    ]
    !seen

let a_settled_step_names_its_root_input () =
  let f = fixture () in
  let steps = ref [] in
  Store.add_invariant f.store "steps" (fun _ step ->
      (match step with
      | Settled_step { execution; category; time; _ } ->
          steps := !steps @ [ (execution, category_string category, time) ]
      | Added | Restored -> ());
      Ok ());
  ignore (Store.dispatch_exn f.store f.tally_m (Add 1));
  Alcotest.(check (list (triple int string int))) "steps" [ (1, "Action", 1_000) ] !steps

let no_checker_runs_after_a_fault () =
  let f = fixture () in
  let seen = ref [] in
  Store.add_invariant f.store "records" (recording seen);
  (match Store.dispatch_to f.store f.tally_m Boom with
  | Error (Faulted _) -> ()
  | other -> Alcotest.failf "expected a fault, got %s" (outcome_string other));
  Alcotest.check strings "never on the half-changed State" [ "added: 0" ] !seen

let a_violation_is_a_bug_report_and_the_store_goes_on () =
  let f = fixture () in
  let ((_, line, _, _) as pos) = __POS__ in
  Store.add_invariant ~pos f.store "below ten" (fun n _ ->
      if n < 10 then Ok () else Error (Printf.sprintf "%d is not below ten" n));
  ignore (Store.dispatch_exn f.store f.tally_m (Add 12));
  ignore (Store.dispatch_exn f.store f.tally_m (Add 1));
  let reports = reports f in
  Alcotest.(check int) "one per root input after which it fails" 2 (List.length reports);
  let report = List.hd reports in
  let place = Option.get report.place in
  Alcotest.(check string) "phase" "Invariant(below ten)" (phase_string place.phase);
  Alcotest.(check (option int)) "at" (Some 1) place.at;
  Alcotest.(check (pair string int))
    "message and line" ("12 is not below ten", line) (report.message, report.line);
  Alcotest.(check bool)
    ("file " ^ report.file) true
    (ends_with ~suffix:"test_invariants.ml" report.file);
  Alcotest.(check int) "state" 13 (Store.state f.store)

let a_checker_that_panics_is_reported_apart_and_disabled () =
  let f = fixture () in
  let calls = ref 0 in
  Store.add_invariant f.store "fragile" (fun n _ ->
      incr calls;
      if n > 3 then failwith (Printf.sprintf "cannot read %d" n);
      Ok ());
  ignore (Store.dispatch_exn f.store f.tally_m (Add 5));
  ignore (Store.dispatch_exn f.store f.tally_m (Add 1));
  match reports f with
  | [ report ] ->
      Alcotest.(check string)
        "phase" "InvariantPanic(fragile)"
        (phase_string (Option.get report.place).phase);
      Alcotest.(check bool)
        report.message true
        (let needle = "cannot read 5" and s = report.message in
         let n = String.length needle in
         let rec find i =
           i + n <= String.length s && (String.sub s i n = needle || find (i + 1))
         in
         find 0);
      Alcotest.(check int) "added, then the panic; disabled after it" 2 !calls
  | rs -> Alcotest.failf "expected one report, got %d" (List.length rs)

let invariants_move_into_the_next_store_with_their_memory () =
  let f = fixture () in
  let seen = ref [] in
  Store.add_invariant f.store "records" (recording seen);
  let most = ref 0 in
  Store.add_invariant f.store "never decreases" (fun n step ->
      if step = Restored then most := 0 (* this application restores from scratch *);
      let broken = n < !most in
      most := max !most n;
      if broken then Error (Printf.sprintf "%d after %d" n !most) else Ok ());
  ignore (Store.dispatch_exn f.store f.tally_m (Add 7));
  let invariants = Store.take_invariants f.store in
  Alcotest.(check int) "taken" 2 (Invariants.length invariants);
  Store.close f.store;
  let g = fixture () in
  Store.restore_invariants g.store invariants;
  ignore (Store.dispatch_exn g.store g.tally_m (Add 1));
  Alcotest.check strings "seen"
    [ "restored: 0"; "Action tally.add at /: 1" ]
    (List.filteri (fun i _ -> i >= 2) !seen);
  Alcotest.(check int) "the restored State is told apart" 0 (List.length (reports g))

(* Taking the invariants leaves the Store none to run; restored into the
   next Store, they run there, and one disabled by its own exception stays
   disabled. *)
let taken_invariants_leave_and_a_disabled_one_stays_disabled () =
  let f = fixture () in
  let calls = ref 0 in
  Store.add_invariant f.store "fragile" (fun n _ ->
      incr calls;
      if n > 3 then failwith "cannot read past 3";
      Ok ());
  let seen = ref [] in
  Store.add_invariant f.store "records" (recording seen);
  ignore (Store.dispatch_exn f.store f.tally_m (Add 5));
  Alcotest.(check int) "added, then the panic" 2 !calls;
  let invariants = Store.take_invariants f.store in
  Alcotest.(check int) "both taken" 2 (Invariants.length invariants);
  Alcotest.(check bool) "taken" false (Invariants.is_empty invariants);
  Alcotest.(check bool)
    "none left behind" true
    (Invariants.is_empty (Store.take_invariants f.store));
  ignore (Store.dispatch_exn f.store f.tally_m (Add 1));
  Alcotest.check strings "the first Store runs none after the take"
    [ "added: 0"; "Action tally.add at /: 5" ]
    !seen;
  Store.close f.store;
  let g = fixture () in
  Store.restore_invariants g.store invariants;
  ignore (Store.dispatch_exn g.store g.tally_m (Add 9));
  Alcotest.check strings "the next Store runs them"
    [ "added: 0"; "Action tally.add at /: 5"; "restored: 0"; "Action tally.add at /: 9" ]
    !seen;
  Alcotest.(check int) "the disabled one stays disabled" 2 !calls;
  Alcotest.(check int) "and reports nothing more" 0 (List.length (reports g))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "invariants"
    [
      ( "invariants",
        [
          t "a checker runs when added and after each settled root input only"
            a_checker_runs_when_added_and_after_each_settled_root_input_only;
          t "a settled step names its root input" a_settled_step_names_its_root_input;
          t "no checker runs after a fault" no_checker_runs_after_a_fault;
          t "a violation is a bug report and the store goes on"
            a_violation_is_a_bug_report_and_the_store_goes_on;
          t "a checker that panics is reported apart and disabled"
            a_checker_that_panics_is_reported_apart_and_disabled;
          t "invariants move into the next store with their memory"
            invariants_move_into_the_next_store_with_their_memory;
          t "taken invariants leave, and a disabled one stays disabled"
            taken_invariants_leave_and_a_disabled_one_stays_disabled;
        ] );
    ]
