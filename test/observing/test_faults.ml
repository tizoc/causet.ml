(* Port of the Rust tests/faults.rs: an exception in the work an Effect's
   [dispatch] runs faults the Store as it leaves that work, whether or not
   the handler catches it: the fault is the exception's, the bus closes, and
   nothing else in the root input runs. *)

open Causet
open Common

(* What the Effect [Run] does once it has dispatched [Note 1]. *)
type then_ =
  | Catch  (** It catches a dispatch's exception and carries on. *)
  | Catch_nested
      (** It dispatches [Nested], whose Effect catches the exception, and catches what
          comes out of its own dispatch. *)
  | Catch_then_raise  (** It catches the exception, then raises its own. *)

type catcher_action = Start of then_ | Nested | Note of int | Boom
type catcher_effect = Run of then_ | After
type ping = Ping

let catcher_actions =
  Message.make ~name:"catcher.action"
    ~kind:(function
      | Start _ -> "catcher.start"
      | Nested -> "catcher.nested"
      | Note _ -> "catcher.note"
      | Boom -> "catcher.boom")
    ()

let catcher_effects =
  Message.make ~name:"catcher.effect"
    ~kind:(function Run _ -> "catcher.run" | After -> "catcher.after")
    ()

let pings = Message.make ~name:"catcher.ping" ~kind:(fun Ping -> "catcher.ping") ()

(* What the handlers saw. *)
type seen = {
  mutable caught : string list;
  mutable after_catch : dispatch_outcome list;
  mutable reply_closed : bool option;
  mutable after_ran : bool;
}

let catcher =
  Component.make ~name:"catcher" ~actions:catcher_actions ~effects:catcher_effects
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Start then_ ->
          Ctx.emit_effect ctx (Run then_);
          Ctx.emit_action ctx (Note 99);
          Ctx.emit_effect ctx After
      | Nested -> Ctx.emit_effect ctx (Run Catch)
      | Note n -> Ctx.update ctx (fun notes -> notes @ [ n ])
      | Boom -> failwith "boom")
    ~handle:(fun ctx eff _ ->
      match eff with
      | After -> (Effect_ctx.service ctx).after_ran <- true
      | Run then_ ->
          if Effect_ctx.dispatch ctx (Note 1) <> Executed then failwith "Note 1 refused";
          let inner = match then_ with Catch_nested -> Nested | _ -> Boom in
          let caught =
            match Effect_ctx.dispatch ctx inner with
            | outcome -> if outcome = Executed then "returned" else "refused"
            | exception Failure m -> m
            | exception _ -> "another exception"
          in
          let after = Effect_ctx.dispatch ctx (Note 2) in
          let reply = Reply.try_send (Effect_ctx.reply ctx pings) Ping in
          let seen = Effect_ctx.service ctx in
          seen.caught <- seen.caught @ [ caught ];
          seen.after_catch <- seen.after_catch @ [ after ];
          seen.reply_closed <-
            Some (match reply with Error (Closed _) -> true | _ -> false);
          if then_ = Catch_then_raise then failwith "the handler's own exception")
    ~register:(fun r ->
      Registrar.event r pings (fun Ping port -> Port.emit_action port (Start Catch)))
    ()

type fixture = {
  store : (int list, seen) Store.t;
  sink : Sink.t;
  mount : (int list, seen, catcher_action, catcher_effect, never) mount;
  log : Observe.Trace_log.log;
  faults : string list ref;
}

let fixture () =
  let reg = Registry.create () in
  Registry.root reg catcher;
  let proto =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "invalid protocol"
  in
  let sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state:[]
      ~services:{ caught = []; after_catch = []; reply_closed = None; after_ran = false }
      inbox
  in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let faults = ref [] in
  Store.add_observer store
    {
      Observe.none with
      fault =
        (fun (f : Observe.Fault_info.t) ->
          faults :=
            !faults
            @ [
                Printf.sprintf "%s at %s: %s" (phase_debug f.phase)
                  (match f.at with Some at -> string_of_int at | None -> "none")
                  f.message;
              ]);
    };
  { store; sink; mount = Protocol.resolve_exn proto (Path.root catcher); log; faults }

(* Every execution started, as (kind, depth). *)
let started f =
  List.map
    (fun (e : Observe.Trace_entry.t) -> Printf.sprintf "%s %d" e.kind e.depth)
    (Observe.Trace_log.entries f.log)

(* The fault, as the observer is told of it: at the Action that raised. *)
let boom_fault f =
  let boom =
    List.find
      (fun (e : Observe.Trace_entry.t) -> e.kind = "catcher.boom")
      (Observe.Trace_log.entries f.log)
  in
  [ Printf.sprintf "Reducer at %d: boom" boom.id ]

let outcome_debug = function
  | Executed -> "Executed"
  | Disabled -> "Disabled"
  | Faulted -> "Faulted"

let error_debug = function
  | Ok _ -> "Ok"
  | Error (Faulted m) -> "Faulted " ^ m
  | Error Foreign_mount -> "Foreign_mount"
  | Error Foreign_bus -> "Foreign_bus"

let strings = Alcotest.(list string)

let a_raise_a_handler_catches_from_its_dispatch_faults_the_store () =
  let f = fixture () in
  let result = Store.dispatch_to f.store f.mount (Start Catch) in
  (* The root input returns the fault: the exception's message, at its place. *)
  Alcotest.(check string) "result" "Faulted boom" (error_debug result);
  Alcotest.(check (option string)) "fault" (Some "boom") (Store.fault f.store);
  Alcotest.check strings "observers are told once" (boom_fault f) !(f.faults);
  (* The handler caught the exception, then found the Store faulted: its
     dispatch ran nothing and its reply found the bus closed. *)
  let seen = Store.services f.store in
  Alcotest.check strings "caught" [ "boom" ] seen.caught;
  Alcotest.check strings "after the catch" [ "Faulted" ]
    (List.map outcome_debug seen.after_catch);
  Alcotest.(check (option bool)) "reply" (Some true) seen.reply_closed;
  (* Nothing ran after the exception: not the handler's dispatch, nor what
     was queued after its Effect. What ran before has its own depth. *)
  Alcotest.(check (list int)) "notes" [ 1 ] (Store.state f.store);
  Alcotest.(check bool) "after" false seen.after_ran;
  Alcotest.check strings "started"
    [ "catcher.start 1"; "catcher.run 2"; "catcher.note 3"; "catcher.boom 3" ]
    (started f);
  (* Later calls are refused as faulted, and the bus is closed. *)
  Alcotest.(check string)
    "later dispatch" "Faulted boom"
    (error_debug (Store.dispatch_to f.store f.mount (Note 3)));
  Alcotest.(check string)
    "later drain" "Faulted boom"
    (error_debug (Store.drain_events f.store 16));
  Alcotest.(check bool)
    "bus closed" true
    (match Sink.try_send_to f.sink f.mount pings Ping with
    | Error (Closed _) -> true
    | _ -> false);
  Alcotest.(check int) "one fault" 1 (List.length !(f.faults))

let every_level_that_catches_it_finds_the_store_faulted () =
  let f = fixture () in
  let result = Store.dispatch_to f.store f.mount (Start Catch_nested) in
  Alcotest.(check string) "result" "Faulted boom" (error_debug result);
  Alcotest.check strings "faults" (boom_fault f) !(f.faults);
  (* The inner handler caught the exception; the outer one caught what ended
     the rest of the inner root input's work, not the exception again. *)
  let seen = Store.services f.store in
  Alcotest.check strings "caught" [ "boom"; "another exception" ] seen.caught;
  Alcotest.check strings "after the catch" [ "Faulted"; "Faulted" ]
    (List.map outcome_debug seen.after_catch);
  Alcotest.(check (list int)) "notes" [ 1; 1 ] (Store.state f.store);
  Alcotest.check strings "started"
    [
      "catcher.start 1";
      "catcher.run 2";
      "catcher.note 3";
      "catcher.nested 3";
      "catcher.run 4";
      "catcher.note 5";
      "catcher.boom 5";
    ]
    (started f)

let the_first_raise_is_the_fault () =
  let f = fixture () in
  let result = Store.dispatch_to f.store f.mount (Start Catch_then_raise) in
  Alcotest.(check string) "result" "Faulted boom" (error_debug result);
  Alcotest.check strings "faults" (boom_fault f) !(f.faults)

let an_event_s_root_input_returns_the_fault_too () =
  let f = fixture () in
  ignore (Store.drain_events f.store 0);
  (* this thread owns the Store lane *)
  ignore (Sink.try_send_to f.sink f.mount pings Ping);
  ignore (Sink.try_send_to f.sink f.mount pings Ping);
  Alcotest.(check string)
    "drain" "Faulted boom"
    (error_debug (Store.drain_events f.store 16));
  (* The second Event never ran. *)
  Alcotest.(check (list int)) "notes" [ 1 ] (Store.state f.store);
  Alcotest.check strings "started"
    [
      "catcher.ping 1";
      "catcher.start 2";
      "catcher.run 3";
      "catcher.note 4";
      "catcher.boom 4";
    ]
    (started f);
  Alcotest.(check int) "one fault" 1 (List.length !(f.faults))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "faults"
    [
      ( "faults",
        [
          t "a raise a handler catches from its dispatch faults the store"
            a_raise_a_handler_catches_from_its_dispatch_faults_the_store;
          t "every level that catches it finds the store faulted"
            every_level_that_catches_it_finds_the_store_faulted;
          t "the first raise is the fault" the_first_raise_is_the_fault;
          t "an Event's root input returns the fault too"
            an_event_s_root_input_returns_the_fault_too;
        ] );
    ]
