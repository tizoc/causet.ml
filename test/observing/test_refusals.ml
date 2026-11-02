(* Ported from the Rust port's [tests/refusals.rs]: observers see every
   refused Action, wherever it was sent from, and every child's Action
   reduced inline. *)

open Causet
open Common

(* ------------------------------------------------------------------------ *)
(* Kid: refuses [Refuse]. *)

type kid_action = Add of int | Refuse

let kid_actions =
  Message.make ~name:"kid.action"
    ~kind:(function Add _ -> "kid.add" | Refuse -> "kid.refuse")
    ~pp:(fun f -> function
      | Add n -> Format.fprintf f "Add(%d)" n
      | Refuse -> Format.pp_print_string f "Refuse")
    ()

let kid : (int, unit, kid_action, never, never) Component.t =
  Component.make ~name:"kid" ~actions:kid_actions ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> action <> Refuse)
    ~reduce:(fun ctx action _ ->
      match action with Add n -> Ctx.update ctx (( + ) n) | Refuse -> ())
    ()

(* ------------------------------------------------------------------------ *)
(* Parent: sends to its kid inline or queued, and refuses [Self_refused]
   itself; its guard refuses an addition over 100. *)

let kid_slot = Child.make "kid" kid

type parent_state = { kid : int; outcomes : outcome list }

type parent_action =
  | Inline of kid_action
  | Queue of kid_action
  | Poke
  | Self_refused  (** Rust's [ParentAction::Refused]. *)

type parent_effect = Poke_effect

let parent_actions =
  Message.make ~name:"parent.action"
    ~kind:(function
      | Inline _ -> "parent.inline"
      | Queue _ -> "parent.queue"
      | Poke -> "parent.poke"
      | Self_refused -> "parent.refused")
    ()

let parent_effects =
  Message.make ~name:"parent.effect" ~kind:(fun Poke_effect -> "parent.poke") ()

let parent : (parent_state, unit, parent_action, parent_effect, never) Component.t =
  Component.make ~name:"parent" ~actions:parent_actions ~effects:parent_effects
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> action <> Self_refused)
    ~reduce:(fun ctx action _ ->
      match action with
      | Inline action ->
          let outcome = Ctx.reduce_child ctx kid_slot action in
          Ctx.update ctx (fun s -> { s with outcomes = s.outcomes @ [ outcome ] })
      | Queue action -> Ctx.to_child ctx kid_slot action
      | Poke -> Ctx.emit_effect ctx Poke_effect
      | Self_refused -> ())
    ~handle:(fun ctx Poke_effect _ ->
      if Effect_ctx.dispatch ctx Self_refused <> Disabled then
        failwith "assertion failed: the refused dispatch should be Disabled")
    ~register:(fun r ->
      Registrar.child r kid_slot
        ~lens:(Lens.field ~get:(fun s -> s.kid) ~set:(fun s kid -> { s with kid }))
        ~guard:(fun _ action _ ->
          match action with Add n -> not (n > 100) | Refuse -> true)
        ())
    ()

type fixture = {
  store : (parent_state, unit) Store.t;
  proto : (parent_state, unit) Protocol.t;
  parent : (parent_state, unit, parent_action, parent_effect, never) mount;
  log : Observe.Trace_log.log;
}

let fixture () =
  let reg = Registry.create () in
  Registry.root reg parent;
  let proto =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "invalid protocol"
  in
  let _sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state:{ kid = 0; outcomes = [] } ~services:() inbox
  in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  { store; proto; parent = Protocol.resolve_exn proto (Path.root parent); log }

let send f action =
  match Store.dispatch_to f.store f.parent action with
  | Ok o -> o
  | Error (Faulted m) -> Alcotest.failf "the Store faulted: %s" m
  | Error _ -> Alcotest.fail "the dispatch failed"

let entries f = Observe.Trace_log.entries f.log

(* The last root execution's ID. *)
let last_root f =
  (List.find (fun (e : Observe.Trace_entry.t) -> e.depth = 1) (List.rev (entries f))).id

(* What a refusal says: where, what, by what and from where. *)
let refusals f =
  List.map
    (fun (r : Observe.Refusal_entry.t) -> (r.path, r.kind, r.by, r.sent))
    (Observe.Trace_log.refusals f.log)

let show_refusal (path, kind, by, sent) =
  Printf.sprintf "(%s, %s, %s, %s)" path kind (by_debug by) (sent_debug sent)

let check_refusals msg expected f =
  Alcotest.(check (list string))
    msg
    (List.map show_refusal expected)
    (List.map show_refusal (refusals f))

let outcome_debug = function Reduced -> "Reduced" | Refused -> "Refused"

let a_refused_root_action_is_told_with_no_execution () =
  let f = fixture () in
  Alcotest.(check bool) "disabled" true (send f Self_refused = Disabled);
  Alcotest.(check int) "no execution" 0 (List.length (entries f));
  check_refusals "refusals" [ ("/", "parent.refused", Observe.Component, Observe.Root) ] f

let a_refused_queued_action_is_told_with_its_origin () =
  let f = fixture () in
  ignore (send f (Queue Refuse));
  let sender = last_root f in
  ignore (send f (Queue (Add 500)));
  let second = last_root f in
  match refusals f with
  | [ (path, kind, by, sent); (_, _, guard, guarded) ] ->
      Alcotest.(check (list string))
        "first refusal"
        [ "/kid"; "kid.refuse"; "Component" ]
        [ path; kind; by_debug by ];
      (match sent with
      | Observe.Queued origin ->
          Alcotest.(check (pair int int))
            "origin" (sender, 0)
            (origin.causal, origin.emission_ordinal)
      | other -> Alcotest.failf "queued: %s" (sent_debug other));
      Alcotest.(check string) "guard" "Guard" (by_debug guard);
      Alcotest.(check (option int))
        "guarded from" (Some second)
        (Observe.sent_execution guarded);
      (* Still no execution of their own: only the two parents ran. *)
      Alcotest.(check int) "two executions" 2 (List.length (entries f));
      Alcotest.(check int) "kid" 0 (Store.state f.store).kid
  | other ->
      Alcotest.failf "two refusals: [%s]"
        (String.concat "; " (List.map show_refusal other))

let a_refused_inline_action_is_told_within_its_parent () =
  let f = fixture () in
  ignore (send f (Inline Refuse));
  let first = last_root f in
  ignore (send f (Inline (Add 500)));
  let second = last_root f in
  check_refusals "refusals"
    [
      ("/kid", "kid.refuse", Observe.Component, Observe.From_inline first);
      ("/kid", "kid.add", Observe.Guard, Observe.From_inline second);
    ]
    f;
  Alcotest.(check (list string))
    "outcomes" [ "Refused"; "Refused" ]
    (List.map outcome_debug (Store.state f.store).outcomes);
  Alcotest.(check int)
    "nothing reduced inline" 0
    (List.length (Observe.Trace_log.inline f.log))

let a_refused_effect_dispatch_is_told_from_its_effect () =
  let f = fixture () in
  ignore (send f Poke);
  let effect_id =
    (List.find (fun (e : Observe.Trace_entry.t) -> e.category = Meta.Effect) (entries f))
      .id
  in
  check_refusals "refusals"
    [ ("/", "parent.refused", Observe.Component, Observe.From_effect effect_id) ]
    f

let an_inline_action_is_told_within_its_parent_with_its_payload () =
  let payloads = ref [] in
  let f = fixture () in
  Store.add_observer f.store
    {
      Observe.none with
      inline_action =
        (fun (inline : Observe.Inline_call.t) payload ->
          let add = Option.get (Message.get kid_actions payload) in
          payloads :=
            !payloads
            @ [ Format.asprintf "%s %s %a" inline.path inline.kind kid_actions.pp add ]);
    };
  ignore (send f (Inline (Add 2)));
  let parent = last_root f in
  (match Observe.Trace_log.inline f.log with
  | [ i ] ->
      Alcotest.(check (triple string string int))
        "inline" ("/kid", "kid.add", parent) (i.path, i.kind, i.within)
  | l -> Alcotest.failf "one inline call, got %d" (List.length l));
  Alcotest.(check (list string)) "payloads" [ "/kid kid.add Add(2)" ] !payloads;
  (* An inline call, so no execution of its own; and a queued one is no
     inline call. *)
  Alcotest.(check int) "one execution" 1 (List.length (entries f));
  ignore (send f (Queue (Add 3)));
  Alcotest.(check int) "still one inline" 1 (List.length (Observe.Trace_log.inline f.log));
  Alcotest.(check int) "kid" 5 (Store.state f.store).kid

(* A raise in either callback faults the Store at its boundary, naming the
   execution that sent the Action. *)
let a_panic_in_either_callback_faults_at_its_boundary () =
  let faults action =
    let seen = ref [] in
    let f = fixture () in
    Store.add_observer f.store
      {
        Observe.none with
        action_refused = (fun _ _ -> failwith "refused");
        inline_action = (fun _ _ -> failwith "inline");
        fault =
          (fun (fault : Observe.Fault_info.t) ->
            seen := !seen @ [ (fault.phase, fault.at) ]);
      };
    (match Store.dispatch_to f.store f.parent action with
    | Error (Faulted _) -> ()
    | _ -> Alcotest.fail "the Store should fault");
    !seen
  in
  let show (phase, at) = Printf.sprintf "(%s, %s)" (phase_debug phase) (at_debug at) in
  Alcotest.(check (list string))
    "root refused"
    [ "(Observer(Refused), None)" ]
    (List.map show (faults Self_refused));
  (match faults (Inline (Add 1)) with
  | [ (phase, at) ] ->
      Alcotest.(check (pair string bool))
        "inline" ("Observer(Inline)", true)
        (phase_debug phase, at <> None)
  | l -> Alcotest.failf "one fault, got %d" (List.length l));
  match faults (Queue Refuse) with
  | [ (phase, at) ] ->
      Alcotest.(check (pair string bool))
        "queued refused" ("Observer(Refused)", true)
        (phase_debug phase, at <> None)
  | l -> Alcotest.failf "one fault, got %d" (List.length l)

let a_refusal_and_an_inline_call_carry_their_mount_type_and_time () =
  let f = fixture () in
  ignore (send f (Queue Refuse));
  let sender = last_root f in
  ignore (send f (Inline (Add 1)));
  let kid_mount = Protocol.resolve_exn f.proto (Path.child (Path.root parent) kid_slot) in
  let instance = Protocol.instance_of kid_mount in
  (match Observe.Trace_log.refusals f.log with
  | [ refusal ] ->
      (* Checked when its turn came: right after its sender, at its time. *)
      let sent_at =
        (List.find (fun (e : Observe.Trace_entry.t) -> e.id = sender) (entries f)).time
      in
      Alcotest.(check int) "time" sent_at refusal.time
  | l -> Alcotest.failf "one refusal, got %d" (List.length l));
  (* An inline call is told at its parent's time. *)
  let parent_id = last_root f in
  let parent_at =
    (List.find (fun (e : Observe.Trace_entry.t) -> e.id = parent_id) (entries f)).time
  in
  (match Observe.Trace_log.inline f.log with
  | [ inline ] ->
      Alcotest.(check (pair int int))
        "within, time" (parent_id, parent_at) (inline.within, inline.time);
      Alcotest.(check bool) "a time, not the parent's ID" true (inline.time <> parent_id)
  | l -> Alcotest.failf "one inline call, got %d" (List.length l));
  let fields = ref [] in
  Store.add_observer f.store
    {
      Observe.none with
      action_refused =
        (fun (r : Observe.Refusal.t) _ -> fields := !fields @ [ (r.instance, r.message) ]);
      inline_action =
        (fun (i : Observe.Inline_call.t) _ ->
          fields := !fields @ [ (i.instance, i.message) ]);
    };
  ignore (send f (Queue Refuse));
  ignore (send f (Inline (Add 1)));
  let show (i, m) = Format.asprintf "(%a, %s)" pp_instance i m in
  Alcotest.(check (list string))
    "fields"
    (List.map show [ (instance, "kid.action"); (instance, "kid.action") ])
    (List.map show !fields)

(* ------------------------------------------------------------------------ *)
(* Top, Mid and Leaf: inline calls two deep, a Leaf whose kind raises from 90
   up, and a Top that catches a raise. *)

(* Its kind is application code that raises from 90 up. *)
type leaf_action = Leaf_action of int

let leaf_actions =
  Message.make ~name:"leaf.action"
    ~kind:(fun (Leaf_action n) ->
      if not (n < 90) then failwith "the kind callback failed";
      "leaf.go")
    ()

(* Refuses 0 and every odd number from 91. *)
let leaf : (int, unit, leaf_action, never, never) Component.t =
  Component.make ~name:"leaf" ~actions:leaf_actions ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun _ (Leaf_action n) _ -> n <> 0 && not (n >= 91 && n mod 2 = 1))
    ~reduce:(fun ctx (Leaf_action n) _ -> Ctx.update ctx (( + ) n))
    ()

type mid_state = { leaf : int }
type mid_action = Mid_inline of int | Mid_queue of int

let mid_actions =
  Message.make ~name:"mid.action"
    ~kind:(function Mid_inline _ -> "mid.inline" | Mid_queue _ -> "mid.queue")
    ()

let leaf_slot = Child.make "leaf" leaf

let mid : (mid_state, unit, mid_action, never, never) Component.t =
  Component.make ~name:"mid" ~actions:mid_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Mid_inline n -> ignore (Ctx.reduce_child ctx leaf_slot (Leaf_action n))
      | Mid_queue n -> Ctx.to_child ctx leaf_slot (Leaf_action n))
    ~register:(fun r ->
      Registrar.child r leaf_slot
        ~lens:
          (Lens.field ~get:(fun (s : mid_state) -> s.leaf) ~set:(fun _ leaf -> { leaf }))
        ())
    ()

type top_state = { mid : mid_state }

(* What a reducer does after catching a raise. *)
type then_ =
  | Nothing
  | Panic  (** Raises with its own text. *)
  | Panic_alike  (** Raises with the very text the observer raised with. *)
  | Panic_with_number  (** Raises with a payload that is not text. *)

let then_debug = function
  | Nothing -> "Nothing"
  | Panic -> "Panic"
  | Panic_alike -> "PanicAlike"
  | Panic_with_number -> "PanicWithNumber"

(* What an observer raises with. *)
type panics = Text | Number

(* Rust's [panic_any(n: u32)]: a payload that is not text. *)
exception Number_payload of int

type top_action =
  | Top_inline of int
  | Top_queue of int
  | Catching of then_  (** Calls inline, catching any raise, then does [then_]. *)
  | Top_panic

let top_actions =
  Message.make ~name:"top.action"
    ~kind:(function
      | Top_inline _ -> "top.inline"
      | Top_queue _ -> "top.queue"
      | Catching _ -> "top.catching"
      | Top_panic -> "top.panic")
    ()

let mid_slot = Child.make "mid" mid

let top : (top_state, unit, top_action, never, never) Component.t =
  Component.make ~name:"top" ~actions:top_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Top_inline n -> ignore (Ctx.reduce_child ctx mid_slot (Mid_inline n))
      | Top_queue n -> ignore (Ctx.reduce_child ctx mid_slot (Mid_queue n))
      | Catching then_ -> (
          let caught =
            match Ctx.reduce_child ctx mid_slot (Mid_inline 1) with
            | _ -> false
            | exception _ -> true
          in
          if not caught then failwith "the observer did not panic";
          match then_ with
          | Nothing -> ()
          | Panic -> failwith "the reducer panics afterwards"
          | Panic_alike -> failwith "the observer panicked"
          | Panic_with_number -> raise (Number_payload 2))
      | Top_panic -> failwith "the observer panicked")
    ~register:(fun r ->
      Registrar.child r mid_slot
        ~lens:(Lens.field ~get:(fun s -> s.mid) ~set:(fun _ mid -> { mid }))
        ())
    ()

let top_store () =
  let reg = Registry.create () in
  Registry.root reg top;
  let proto =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "invalid protocol"
  in
  let _sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto
      ~state:{ mid = { leaf = 0 } }
      ~services:() inbox
  in
  (store, Protocol.resolve_exn proto (Path.root top))

(* Records faults; raises in [inline_action] if told to. *)
let watcher faults panics =
  {
    Observe.none with
    inline_action =
      (fun _ _ ->
        match panics with
        | None -> ()
        | Some Text -> failwith "the observer panicked"
        | Some Number -> raise (Number_payload 1));
    fault =
      (fun (fault : Observe.Fault_info.t) ->
        faults := !faults @ [ (fault.phase, fault.at, fault.message) ]);
  }

let traced store =
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  log

let inline_calls_two_deep_are_told_within_the_root_execution () =
  let store, top = top_store () in
  let trace = traced store in
  ignore (Store.dispatch_exn store top (Top_inline 2));
  ignore (Store.dispatch_exn store top (Top_inline 0));
  match Observe.Trace_log.entries trace with
  | [ first; second ] ->
      Alcotest.(check (list (triple string string int)))
        "inline"
        [
          ("/mid", "mid.inline", first.id);
          ("/mid/leaf", "leaf.go", first.id);
          ("/mid", "mid.inline", second.id);
        ]
        (List.map
           (fun (i : Observe.Inline_entry.t) -> (i.path, i.kind, i.within))
           (Observe.Trace_log.inline trace));
      (match Observe.Trace_log.refusals trace with
      | [ refusal ] ->
          Alcotest.(check (triple string string string))
            "refusal"
            ("/mid/leaf", "Component", sent_debug (Observe.From_inline second.id))
            (refusal.path, by_debug refusal.by, sent_debug refusal.sent)
      | l -> Alcotest.failf "one refusal, got %d" (List.length l));
      Alcotest.(check int) "leaf" 2 (Store.state store).mid.leaf
  | l -> Alcotest.failf "two executions, got %d" (List.length l)

(* A refused Action's kind is read only to tell an observer of it, and an
   inline Action's only to tell one of the call: with none, a kind that
   raises changes nothing. *)
let with_no_observer_a_kind_is_not_read_to_tell_one () =
  let store, top = top_store () in
  (* Admitted inline, refused inline, refused queued. *)
  List.iter
    (fun action ->
      match Store.dispatch_to store top action with
      | Ok Executed -> ()
      | Ok Disabled -> Alcotest.fail "expected Executed, got Disabled"
      | Ok Faulted -> Alcotest.fail "expected Executed, got Faulted"
      | Error (Faulted m) -> Alcotest.failf "expected Executed, the Store faulted: %s" m
      | Error _ -> Alcotest.fail "expected Executed")
    [ Top_inline 90; Top_inline 91; Top_queue 93 ];
  Alcotest.(check int) "leaf" 90 (Store.state store).mid.leaf

(* With an observer, reading the kind is part of telling it: a kind that
   raises faults the Store as that observer boundary's fault. *)
let with_an_observer_a_kind_that_panics_faults_at_its_boundary () =
  let faults action =
    let store, top = top_store () in
    let seen = ref [] in
    let trace = traced store in
    Store.add_observer store (watcher seen None);
    Alcotest.(check bool)
      "faulted" true
      (Result.is_error (Store.dispatch_to store top action));
    let root = (List.hd (Observe.Trace_log.entries trace)).id in
    match !seen with
    | (phase, at, _) :: _ -> (phase_debug phase, at = Some root)
    | [] -> Alcotest.fail "no fault seen"
  in
  Alcotest.(check (pair string bool))
    "admitted inline" ("Observer(Inline)", true) (faults (Top_inline 90));
  Alcotest.(check (pair string bool))
    "refused inline" ("Observer(Refused)", true) (faults (Top_inline 91));
  (* Queued by Mid, inline within the root execution: its refusal names that
     execution too. *)
  Alcotest.(check (pair string bool))
    "refused queued" ("Observer(Refused)", true) (faults (Top_queue 93))

(* The first fault's phase and message, after [actions] with an observer that
   raises with [panics]. *)
let first_fault panics actions =
  let store, top = top_store () in
  let seen = ref [] in
  Store.add_observer store (watcher seen (Some panics));
  List.iter (fun action -> ignore (Store.dispatch_to store top action)) actions;
  match !seen with
  | (phase, _, message) :: _ -> (phase_debug phase, message)
  | [] -> Alcotest.fail "no fault seen"

(* A reducer that catches an observer's raise swallows it, as it would any
   raise; a later fault is then placed where it happens, not at the observer,
   whatever its payload. Split per case of the Rust test so that each
   difference shows on its own. *)
let caught_then panics then_ message () =
  Alcotest.(check (pair string string))
    (then_debug then_) ("Reducer", message)
    (first_fault panics [ Catching then_ ])

let caught_and_swallowed_then_a_later_fault_is_at_its_reducer () =
  (* A later root input faulting with the very same text as the observer's is
     placed at its reducer. *)
  Alcotest.(check string)
    "phase" "Reducer"
    (fst (first_fault Text [ Catching Nothing; Top_panic ]))

let uncaught_is_the_observers_fault panics message () =
  Alcotest.(check (pair string string))
    "fault" ("Observer(Inline)", message)
    (first_fault panics [ Top_inline 1 ])

(* A bug an observer reports while told of an inline call or a refusal is
   placed at that boundary, in the execution that sent the Action, at the
   child's mount. *)
let a_bug_an_observer_reports_is_placed_at_its_boundary () =
  let store, top = top_store () in
  let trace = traced store in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer bugs);
  Store.add_observer store
    {
      Observe.none with
      inline_action = (fun _ _ -> Bug.condition __POS__ "seen inline");
      action_refused = (fun _ _ -> Bug.condition __POS__ "seen refused");
    };
  ignore (Store.dispatch_exn store top (Top_queue 0));
  let root = (List.hd (Observe.Trace_log.entries trace)).id in
  let places =
    List.map
      (fun (r : Observe.Bug_report.t) ->
        match r.place with
        | None -> Printf.sprintf "(%s, no place)" r.message
        | Some place ->
            Printf.sprintf "(%s, %s, %s, %s)" r.message (phase_debug place.phase)
              (at_debug place.at) place.path)
      (Observe.Bug_log.reports bugs)
  in
  Alcotest.(check (list string))
    "places"
    [
      Printf.sprintf "(seen inline, Observer(Inline), %s, /mid)" (at_debug (Some root));
      Printf.sprintf "(seen refused, Observer(Refused), %s, /mid/leaf)"
        (at_debug (Some root));
    ]
    places

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "refusals"
    [
      ( "refusals",
        [
          t "a refused root action is told with no execution"
            a_refused_root_action_is_told_with_no_execution;
          t "a refused queued action is told with its origin"
            a_refused_queued_action_is_told_with_its_origin;
          t "a refused inline action is told within its parent"
            a_refused_inline_action_is_told_within_its_parent;
          t "a refused effect dispatch is told from its effect"
            a_refused_effect_dispatch_is_told_from_its_effect;
          t "an inline action is told within its parent with its payload"
            an_inline_action_is_told_within_its_parent_with_its_payload;
          t "a panic in either callback faults at its boundary"
            a_panic_in_either_callback_faults_at_its_boundary;
          t "a refusal and an inline call carry their mount, type and time"
            a_refusal_and_an_inline_call_carry_their_mount_type_and_time;
        ] );
      ( "inline two deep",
        [
          t "inline calls two deep are told within the root execution"
            inline_calls_two_deep_are_told_within_the_root_execution;
          t "with no observer a kind is not read to tell one"
            with_no_observer_a_kind_is_not_read_to_tell_one;
          t "with an observer a kind that panics faults at its boundary"
            with_an_observer_a_kind_that_panics_faults_at_its_boundary;
          t "an observer panic a reducer catches leaves no stale place: then Panic"
            (caught_then Text Panic "the reducer panics afterwards");
          t "an observer panic a reducer catches leaves no stale place: then PanicAlike"
            (caught_then Text Panic_alike "the observer panicked");
          t
            "an observer panic a reducer catches leaves no stale place: then \
             PanicWithNumber"
            (* An exception with no text is named by [Printexc]: OCaml can print
               any, where Rust words an unknown payload "panic". *)
            (caught_then Number Panic_with_number (Printexc.to_string (Number_payload 2)));
          t
            "an observer panic a reducer catches leaves no stale place: swallowed, then \
             a later fault"
            caught_and_swallowed_then_a_later_fault_is_at_its_reducer;
          t "an observer panic a reducer catches leaves no stale place: uncaught text"
            (uncaught_is_the_observers_fault Text "the observer panicked");
          t "an observer panic a reducer catches leaves no stale place: uncaught number"
            (uncaught_is_the_observers_fault Number
               (Printexc.to_string (Number_payload 1)));
          t "a bug an observer reports is placed at its boundary"
            a_bug_an_observer_reports_is_placed_at_its_boundary;
        ] );
    ]
