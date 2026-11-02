(* The kernel's basics: admission, a reducer's State, inline and queued child
   calls, Outputs, request IDs and the order executions run in. *)

open Causet

(* The README's counter: it refuses an addition past 100. *)
type counter_action = Add of int | Reset

let counter_actions =
  Message.make ~name:"counter.action"
    ~kind:(function Add _ -> "counter.add" | Reset -> "counter.reset")
    ()

let counter =
  Component.make ~name:"counter" ~actions:counter_actions ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun total action _ ->
      match action with Add n -> total + n <= 100 | Reset -> true)
    ~reduce:(fun ctx action _ ->
      match action with Add n -> Ctx.update ctx (( + ) n) | Reset -> Ctx.set_state ctx 0)
    ()

let store_of ?observers ~state comp =
  let reg = Registry.create () in
  Registry.root reg comp;
  let proto = match Registry.freeze reg with Ok p -> p | Error _ -> assert false in
  let _sink, inbox = Event_bus.with_session proto ~worker_capacity:16 1 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state ~services:() inbox
  in
  Option.iter (List.iter (Store.add_observer store)) observers;
  (proto, store)

let admission_refuses_and_runs_nothing () =
  let proto, store = store_of ~state:0 counter in
  let m = Protocol.resolve_exn proto (Path.root counter) in
  Alcotest.(check bool) "60 added" true (Store.dispatch_to store m (Add 60) = Ok Executed);
  Alcotest.(check bool)
    "60 more refused" true
    (Store.dispatch_to store m (Add 60) = Ok Disabled);
  Alcotest.(check int) "state" 60 (Store.state store);
  Alcotest.(check int) "one execution" 1 (Store.executions store)

(* A parent with a kid: inline and queued calls, the kid's Output lowered to
   the parent. *)
type kid_action = Bump of int
type kid_output = Bumped of int
type parent_action = Inline of int | Queued of int | Saw of int
type parent_state = { kid : int; seen : int list; outcomes : outcome list }

let kid_actions = Message.make ~name:"kid.action" ~kind:(fun (Bump _) -> "kid.bump") ()

let kid_outputs =
  Message.make ~name:"kid.output" ~kind:(fun (Bumped _) -> "kid.bumped") ()

let kid =
  Component.make ~name:"kid" ~actions:kid_actions ~effects:Message.never
    ~outputs:kid_outputs
    ~is_enabled:(fun _ (Bump n) _ -> n > 0)
    ~reduce:(fun ctx (Bump n) _ ->
      Ctx.update ctx (( + ) n);
      Ctx.emit_output ctx (Bumped (Ctx.state ctx)))
    ()

let parent_actions =
  Message.make ~name:"parent.action"
    ~kind:(function
      | Inline _ -> "parent.inline" | Queued _ -> "parent.queued" | Saw _ -> "parent.saw")
    ()

let rec parent =
  lazy
    (Component.make ~name:"parent" ~actions:parent_actions ~effects:Message.never
       ~outputs:Message.never
       ~reduce:(fun ctx action _ ->
         match action with
         | Inline n ->
             let outcome = Ctx.reduce_child ctx (Lazy.force slot) (Bump n) in
             Ctx.update ctx (fun s -> { s with outcomes = s.outcomes @ [ outcome ] })
         | Queued n -> Ctx.to_child ctx (Lazy.force slot) (Bump n)
         | Saw v -> Ctx.update ctx (fun s -> { s with seen = s.seen @ [ v ] }))
       ~register:(fun r ->
         Registrar.child r (Lazy.force slot)
           ~lens:(Lens.field ~get:(fun s -> s.kid) ~set:(fun s kid -> { s with kid }))
           ~on_output:(fun _ (Bumped v) port -> Port.emit_action port (Saw v))
           ())
       ())

and slot = lazy (Child.make "kid" kid)

let inline_and_queued_child_calls () =
  let log = Observe.Trace_log.create () in
  let parent = Lazy.force parent in
  let proto, store =
    store_of
      ~observers:[ Observe.Trace_log.observer log ]
      ~state:{ kid = 0; seen = []; outcomes = [] }
      parent
  in
  let m = Protocol.resolve_exn proto (Path.root parent) in
  ignore (Store.dispatch_exn store m (Inline 2));
  ignore (Store.dispatch_exn store m (Inline 0));
  ignore (Store.dispatch_exn store m (Queued 3));
  let s = Store.state store in
  Alcotest.(check int) "kid" 5 s.kid;
  Alcotest.(check (list int)) "seen" [ 2; 5 ] s.seen;
  Alcotest.(check bool) "outcomes" true (s.outcomes = [ Reduced; Refused ]);
  let trace =
    List.map
      (fun (e : Observe.Trace_entry.t) ->
        Printf.sprintf "%d %s %s <%d d%d" e.id e.path e.kind e.causal e.depth)
      (Observe.Trace_log.entries log)
  in
  Alcotest.(check (list string))
    "trace"
    [
      "1 / parent.inline <1 d1";
      "2 / parent.saw <1 d2";
      "3 / parent.inline <3 d1";
      "4 / parent.queued <4 d1";
      "5 /kid kid.bump <4 d2";
      "6 / parent.saw <5 d3";
    ]
    trace;
  Alcotest.(check int)
    "one inline call told, one refusal" 1
    (List.length (Observe.Trace_log.inline log));
  Alcotest.(check int) "refusals" 1 (List.length (Observe.Trace_log.refusals log))

let state_after_emitting_is_refused () =
  let bad =
    Component.make ~name:"bad" ~actions:counter_actions ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun ctx _ _ ->
        Ctx.emit_action ctx Reset;
        Ctx.set_state ctx 1)
      ()
  in
  let proto, store = store_of ~state:0 bad in
  let m = Protocol.resolve_exn proto (Path.root bad) in
  match Store.dispatch_to store m (Add 1) with
  | Error (Faulted message) ->
      Alcotest.(check string)
        "message" "causet: set_state after the reducer started emitting" message
  | _ -> Alcotest.fail "the Store should fault"

(* State set before a raise is kept, in a reducer and in an inline child:
   it reaches the Store as the reducer returns or raises, and a child's
   reaches its parent's as the child does, caught or not. *)
type raiser_action = Set_then_raise | Child_raises of { caught : bool }
type raiser_state = { own : int; child : int }

let raiser_actions =
  Message.make ~name:"raiser.action"
    ~kind:(function
      | Set_then_raise -> "raiser.set_then_raise"
      | Child_raises _ -> "raiser.child_raises")
    ()

let breaking_kid =
  Component.make ~name:"breaking_kid" ~actions:kid_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Bump n) _ ->
      Ctx.set_state ctx n;
      failwith "the kid broke")
    ()

let breaking_slot = Child.make "kid" breaking_kid

let raiser =
  Component.make ~name:"raiser" ~actions:raiser_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Set_then_raise ->
          Ctx.update ctx (fun s -> { s with own = 7 });
          failwith "the parent broke"
      | Child_raises { caught } ->
          Ctx.update ctx (fun s -> { s with own = 1 });
          if caught then (
            (try ignore (Ctx.reduce_child ctx breaking_slot (Bump 9))
             with Failure _ -> ());
            Ctx.update ctx (fun s -> { s with own = s.own + 1 }))
          else ignore (Ctx.reduce_child ctx breaking_slot (Bump 9)))
    ~register:(fun r ->
      Registrar.child r breaking_slot
        ~lens:(Lens.field ~get:(fun s -> s.child) ~set:(fun s child -> { s with child }))
        ())
    ()

let state_set_before_a_raise_is_kept () =
  let run action =
    let proto, store = store_of ~state:{ own = 0; child = 0 } raiser in
    let m = Protocol.resolve_exn proto (Path.root raiser) in
    let faulted =
      match Store.dispatch_to store m action with Error (Faulted _) -> true | _ -> false
    in
    let s = Store.state store in
    (faulted, s.own, s.child)
  in
  let check name expected got =
    Alcotest.(check (triple bool int int)) name expected got
  in
  check "the reducer raises" (true, 7, 0) (run Set_then_raise);
  check "the child raises, uncaught" (true, 1, 9) (run (Child_raises { caught = false }));
  check "the child raises, caught" (false, 2, 9) (run (Child_raises { caught = true }))

let () =
  Alcotest.run "kernel"
    [
      ( "kernel",
        [
          Alcotest.test_case "admission refuses and runs nothing" `Quick
            admission_refuses_and_runs_nothing;
          Alcotest.test_case "inline and queued child calls" `Quick
            inline_and_queued_child_calls;
          Alcotest.test_case "State after emitting faults" `Quick
            state_after_emitting_is_refused;
          Alcotest.test_case "State set before a raise is kept" `Quick
            state_set_before_a_raise_is_kept;
        ] );
    ]
