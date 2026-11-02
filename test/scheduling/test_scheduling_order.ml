(* Port of the Rust tests/scheduling_order.rs.

   The order work runs in, and the metadata each execution carries: one FIFO
   per execution, mixing Actions and Effects in emission order; fresh,
   globally linked metadata for each item; an Effect's dispatched Action
   completing before the outer FIFO resumes; Output lowering keeping emission
   order; refused items keeping their reserved emission ordinals; a child
   Action starting its own causal sequence.

   The observer tests at the end check that lifecycle observers wrap their
   handlers only, that a refused Action reaches none of them, that
   [action_settled] runs after the whole queued FIFO, and that observers run
   in the order added until [clear_observers] removes them. *)

open Causet
open Common

(* ------------------------------------------------------------------------ *)
(* The root's protocol *)

type test_action =
  | Root_mixed
  | Followup
  | Root_nested
  | Root_component_fanout
  | Root_disabled_gap
  | Root_child_effect
  | Child_emits_effect
  | Nested
  | Outer_queued
  | Disabled_action  (** [TestAction::Disabled] in Rust; refused by the root. *)

let test_actions =
  Message.make ~name:"test.action"
    ~kind:(function
      | Root_mixed -> "test.root_mixed"
      | Followup -> "test.followup"
      | Root_nested -> "test.root_nested"
      | Root_component_fanout -> "test.root_component_fanout"
      | Root_disabled_gap -> "test.root_disabled_gap"
      | Root_child_effect -> "test.root_child_effect"
      | Child_emits_effect -> "test.child_emits_effect"
      | Nested -> "test.nested"
      | Outer_queued -> "test.outer_queued"
      | Disabled_action -> "test.disabled")
    ()

type test_effect = First | Second | Dispatch_nested

let test_effects =
  Message.make ~name:"test.effect"
    ~kind:(function
      | First -> "test.first"
      | Second -> "test.second"
      | Dispatch_nested -> "test.dispatch_nested")
    ()

(* ------------------------------------------------------------------------ *)
(* The component, mounted at /kid *)

(* [Emit] is what the root asks of it inline: emit Action, Output, Effect. *)
type kid_action = Emit | Kid_followup
type kid_effect = Kid_second
type kid_output = Fanout

let kid_actions =
  Message.make ~name:"kid.action"
    ~kind:(function Emit -> "kid.emit" | Kid_followup -> "kid.followup")
    ()

let kid_effects =
  Message.make ~name:"kid.effect" ~kind:(fun Kid_second -> "kid.second") ()

let kid_outputs = Message.make ~name:"kid.output" ~kind:(fun Fanout -> "kid.fanout") ()

type kid_service = { mutable handled : int }

let kid : (unit, kid_service, kid_action, kid_effect, kid_output) Component.t =
  Component.make ~name:"kid" ~actions:kid_actions ~effects:kid_effects
    ~outputs:kid_outputs
    ~reduce:(fun ctx action _ ->
      match action with
      | Emit ->
          (* An Action, an Output and an Effect, in that order. *)
          Ctx.emit_action ctx Kid_followup;
          Ctx.emit_output ctx Fanout;
          Ctx.emit_effect ctx Kid_second
      | Kid_followup -> ())
    ~handle:(fun ctx Kid_second _ ->
      let svc = Effect_ctx.service ctx in
      svc.handled <- svc.handled + 1)
    ()

let kid_slot = Child.make "kid" kid

(* ------------------------------------------------------------------------ *)
(* The root: its reducer and Effect handler *)

type test_state = { reductions : int; kid : unit }

type test_service = {
  mutable handled_effects : int;
  mutable state_reductions_seen : int list;
  kid_service : kid_service;
}

let test_root : (test_state, test_service, test_action, test_effect, never) Component.t =
  Component.make ~name:"test_root" ~actions:test_actions ~effects:test_effects
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> action <> Disabled_action)
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (fun s -> { s with reductions = s.reductions + 1 });
      match action with
      | Root_mixed ->
          Ctx.emit_effect ctx First;
          Ctx.emit_action ctx Followup;
          Ctx.emit_effect ctx Second
      | Root_nested ->
          Ctx.emit_effect ctx Dispatch_nested;
          Ctx.emit_action ctx Outer_queued
      | Root_component_fanout -> ignore (Ctx.reduce_child ctx kid_slot Emit)
      | Root_disabled_gap ->
          Ctx.emit_action ctx Disabled_action;
          Ctx.emit_effect ctx Second
      | Root_child_effect -> Ctx.emit_action ctx Child_emits_effect
      | Child_emits_effect -> Ctx.emit_effect ctx Second
      | Followup | Nested | Outer_queued | Disabled_action -> ())
    ~handle:(fun ctx eff _ ->
      let reductions =
        match Effect_ctx.state ctx with
        | Some s -> s.reductions
        | None -> failwith "root State"
      in
      let service = Effect_ctx.service ctx in
      service.handled_effects <- service.handled_effects + 1;
      service.state_reductions_seen <- service.state_reductions_seen @ [ reductions ];
      if eff = Dispatch_nested then
        if Effect_ctx.dispatch ctx Nested <> Executed then failwith "Nested was refused")
    ~register:(fun r ->
      Registrar.child r kid_slot
        ~lens:(Lens.field ~get:(fun s -> s.kid) ~set:(fun s kid -> { s with kid }))
        ~service:(fun svc -> svc.kid_service)
        ~on_output:(fun _ Fanout port ->
          (* Lowers the kid's Output into the root's Actions. *)
          Port.emit_action port Nested;
          Port.emit_action port Outer_queued)
        ())
    ()

(* ------------------------------------------------------------------------ *)
(* Harness *)

type fixture = {
  store : (test_state, test_service) Store.t;
  log : Observe.Trace_log.log;
  root : (test_state, test_service, test_action, test_effect, never) mount;
  root_instance : instance_id;
  kid_instance : instance_id;
}

let make_store () =
  let reg = Registry.create () in
  Registry.root reg test_root;
  let p = freeze_exn reg in
  let _sink, inbox = Event_bus.create p ~worker_capacity:4 in
  let store =
    Store.create_exn p ~state:{ reductions = 0; kid = () }
      ~services:
        { handled_effects = 0; state_reductions_seen = []; kid_service = { handled = 0 } }
      inbox
  in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let kid = Protocol.resolve_exn p (Path.child (Path.root test_root) kid_slot) in
  let root = Protocol.resolve_exn p (Path.root test_root) in
  {
    store;
    log;
    root;
    root_instance = Protocol.instance_of root;
    kid_instance = Protocol.instance_of kid;
  }

(* One entry per executed Action or Effect. *)
let executions f = Observe.Trace_log.entries f.log
let cat = function Meta.Action -> "A" | Meta.Effect -> "E" | Meta.Event -> "V"

let order f =
  List.map
    (fun (e : Observe.Trace_entry.t) -> Printf.sprintf "%s %s" (cat e.category) e.kind)
    (executions f)

(* (causal, component, emission ordinal, lowering ordinal); a root input has
   no origin and is self-caused. *)
let provenance (entry : Observe.Trace_entry.t) =
  match entry.origin with
  | Some o -> (o.causal, Some o.component, o.emission_ordinal, o.lowering_ordinal)
  | None -> (entry.causal, None, 0, 0)

let pp_provenance f (causal, component, emission, lowering) =
  Format.fprintf f "(%d, %s, %d, %d)" causal
    (match component with None -> "None" | Some c -> Format.asprintf "%a" pp_instance c)
    emission lowering

let pp_strings = pp_list Format.pp_print_string
let pp_ints = pp_list Format.pp_print_int
let pp_pair f (a, b) = Format.fprintf f "(%d, %d)" a b

let dispatch f action =
  check_dispatch "dispatch" (Ok Executed) (Store.dispatch_to f.store f.root action)

(* ------------------------------------------------------------------------ *)

let typed_dispatcher_preserves_one_mixed_fifo () =
  let f = make_store () in
  check_dispatch "executed" (Ok Executed) (Store.dispatch_to f.store f.root Root_mixed);
  eq ~pp:pp_strings "order"
    [ "A test.root_mixed"; "E test.first"; "A test.followup"; "E test.second" ]
    (order f);
  eq ~pp:Format.pp_print_int "reductions" 2 (Store.state f.store).reductions;
  eq ~pp:Format.pp_print_int "handled effects" 2 (Store.services f.store).handled_effects

let typed_items_receive_fresh_globally_linked_metadata () =
  let f = make_store () in
  dispatch f Root_mixed;
  let executions = executions f in
  eq ~pp:Format.pp_print_int "four executions" 4 (List.length executions);
  eq ~pp:pp_ints "depths" [ 1; 2; 2; 2 ]
    (List.map (fun (e : Observe.Trace_entry.t) -> e.depth) executions);
  let rec pairs = function
    | (a : Observe.Trace_entry.t) :: (b :: _ as rest) ->
        Alcotest.(check bool) "ids increase" true (b.id > a.id);
        (* IDs count executions, in the order they start. *)
        eq ~pp:Format.pp_print_int "counted" (a.id + 1) b.id;
        pairs rest
    | _ -> ()
  in
  pairs executions;
  let root = List.hd executions in
  eq ~pp:Format.pp_print_int "root is self-caused" root.id root.causal;
  eq ~pp:pp_provenance "root provenance" (root.id, None, 0, 0) (provenance root);
  eq ~pp:(pp_list pp_provenance) "queued provenance"
    [
      (root.id, Some f.root_instance, 0, 0);
      (root.id, Some f.root_instance, 1, 0);
      (root.id, Some f.root_instance, 2, 0);
    ]
    (List.map provenance (List.tl executions))

let pooled_dispatcher_resets_each_root_emission_sequence () =
  let f = make_store () in
  dispatch f Root_mixed;
  dispatch f Root_mixed;
  match List.filteri (fun i _ -> i >= 4) (executions f) with
  | [ root; first; followup; second ] ->
      eq ~pp:Format.pp_print_int "root is self-caused" root.id root.causal;
      Alcotest.(check bool) "root has no origin" true (root.origin = None);
      eq ~pp:(pp_list pp_pair) "causes and ordinals"
        [ (root.id, 0); (root.id, 1); (root.id, 2) ]
        (List.map
           (fun e ->
             let c, _, o, _ = provenance e in
             (c, o))
           [ first; followup; second ])
  | _ -> Alcotest.fail "expected the second root and its three queued items"

let effect_dispatched_action_completes_before_outer_fifo_resumes () =
  let f = make_store () in
  dispatch f Root_nested;
  let executions = executions f in
  eq ~pp:pp_strings "order and depths"
    [
      "A test.root_nested 1";
      "E test.dispatch_nested 2";
      "A test.nested 3";
      "A test.outer_queued 2";
    ]
    (List.map
       (fun (e : Observe.Trace_entry.t) ->
         Printf.sprintf "%s %s %d" (cat e.category) e.kind e.depth)
       executions);
  eq ~pp:Format.pp_print_int "reductions" 3 (Store.state f.store).reductions;
  eq ~pp:Format.pp_print_int "handled effects" 1 (Store.services f.store).handled_effects;
  eq ~pp:pp_ints "reductions seen" [ 1 ] (Store.services f.store).state_reductions_seen;
  match executions with
  | [ root; eff; nested; outer ] ->
      let co e =
        let c, _, o, _ = provenance e in
        (c, o)
      in
      eq ~pp:Format.pp_print_int "root is self-caused" root.id root.causal;
      eq ~pp:pp_pair "effect" (root.id, 0) (co eff);
      eq ~pp:pp_pair "nested" (eff.id, 0) (co nested);
      eq ~pp:pp_pair "outer" (root.id, 1) (co outer);
      (* It runs right after the nested Action, which did not cause it. *)
      eq ~pp:Format.pp_print_int "outer follows nested" (nested.id + 1) outer.id;
      Alcotest.(check bool) "nested is not its cause" true (nested.id <> outer.causal)
  | _ -> Alcotest.fail "expected four executions"

let component_output_fanout_retains_one_emission_and_lowering_order () =
  let f = make_store () in
  dispatch f Root_component_fanout;
  match executions f with
  | [ root; first; lowered_action; second_lowered_action; last ] ->
      eq ~pp:Format.pp_print_int "root is self-caused" root.id root.causal;
      let rest = [ first; lowered_action; second_lowered_action; last ] in
      eq ~pp:pp_strings "kinds and paths"
        [
          "A kid.followup /kid";
          "A test.nested /";
          "A test.outer_queued /";
          "E kid.second /kid";
        ]
        (List.map
           (fun (e : Observe.Trace_entry.t) ->
             Printf.sprintf "%s %s %s" (cat e.category) e.kind e.path)
           rest);
      eq ~pp:(pp_list pp_provenance) "provenance"
        [
          (root.id, Some f.kid_instance, 0, 0);
          (root.id, Some f.kid_instance, 1, 0);
          (root.id, Some f.kid_instance, 1, 1);
          (root.id, Some f.kid_instance, 2, 0);
        ]
        (List.map provenance rest);
      eq ~pp:Format.pp_print_int "kid handled" 1
        (Store.services f.store).kid_service.handled
  | executions ->
      Alcotest.failf "expected root and four component-lowered executions, got %d"
        (List.length executions)

let drain_disabled_item_keeps_its_reserved_emission_gap () =
  let f = make_store () in
  dispatch f Root_disabled_gap;
  match executions f with
  | [ root; enabled_effect ] ->
      eq ~pp:Format.pp_print_string "the enabled Effect" "E test.second"
        (Printf.sprintf "%s %s" (cat enabled_effect.category) enabled_effect.kind);
      (* The refused Action took no ID. *)
      eq ~pp:Format.pp_print_int "counted" (root.id + 1) enabled_effect.id;
      eq ~pp:Format.pp_print_int "causal" root.id enabled_effect.causal;
      let _, _, ordinal, _ = provenance enabled_effect in
      eq ~pp:Format.pp_print_int "ordinal keeps the gap" 1 ordinal
  | _ -> Alcotest.fail "the disabled Action must not allocate execution metadata"

let child_action_starts_a_fresh_causal_emission_sequence () =
  let f = make_store () in
  dispatch f Root_child_effect;
  match executions f with
  | [ root; child; eff ] ->
      let co e =
        let c, _, o, _ = provenance e in
        (c, o)
      in
      eq ~pp:pp_pair "child" (root.id, 0) (co child);
      eq ~pp:pp_pair "effect" (child.id, 0) (co eff)
  | _ -> Alcotest.fail "expected root Action, child Action, and child Effect"

let disabled_action_invokes_no_lifecycle_observer () =
  let f = make_store () in
  check_dispatch "disabled" (Ok Disabled)
    (Store.dispatch_to f.store f.root Disabled_action);
  eq ~pp:Format.pp_print_int "no executions" 0 (List.length (executions f))

(* ------------------------------------------------------------------------ *)
(* Observers *)

(* Every lifecycle callback, in order, with the execution's kind and ID. *)
type lifecycle = {
  mutable seen : (string * string * execution_id) list (* newest first *);
}

let lifecycle_observer l =
  let push call (e : Observe.Execution.t) =
    l.seen <- (call, e.kind, e.meta.id) :: l.seen
  in
  {
    Observe.none with
    action_start = (fun e _ -> push "action_start" e);
    action_finish = push "action_finish";
    action_settled = push "action_settled";
    effect_start = (fun e _ -> push "effect_start" e);
    effect_finish = push "effect_finish";
  }

let seen l = List.rev l.seen
let calls l = List.map (fun (call, kind, _) -> call ^ " " ^ kind) (seen l)

let lifecycle_observers_wrap_only_their_typed_handlers () =
  let f = make_store () in
  let l = { seen = [] } in
  Store.add_observer f.store (lifecycle_observer l);
  dispatch f Root_mixed;
  eq ~pp:pp_strings "calls"
    [
      "action_start test.root_mixed";
      "action_finish test.root_mixed";
      "effect_start test.first";
      "effect_finish test.first";
      "action_start test.followup";
      "action_finish test.followup";
      "action_settled test.followup";
      "effect_start test.second";
      "effect_finish test.second";
      "action_settled test.root_mixed";
    ]
    (calls l);
  (* Each pair of callbacks names one execution. *)
  let ends_with ~suffix s =
    let n = String.length suffix and m = String.length s in
    m >= n && String.sub s (m - n) n = suffix
  in
  let rec pairs = function
    | (start, _, a) :: ((finish, _, b) :: _ as rest) ->
        if ends_with ~suffix:"_start" start && ends_with ~suffix:"_finish" finish then
          eq ~pp:Format.pp_print_int "one execution" a b;
        pairs rest
    | _ -> ()
  in
  pairs (seen l)

let action_settled_observer_runs_after_the_entire_queued_fifo () =
  let f = make_store () in
  let l = { seen = [] } in
  Store.add_observer f.store (lifecycle_observer l);
  dispatch f Root_mixed;
  let entries = executions f in
  let id_of kind =
    match List.find_opt (fun (e : Observe.Trace_entry.t) -> e.kind = kind) entries with
    | Some e -> e.id
    | None -> Alcotest.fail "executed"
  in
  let settled =
    List.filter_map
      (fun (call, _, id) -> if call = "action_settled" then Some id else None)
      (seen l)
  in
  eq ~pp:pp_ints "settled" [ id_of "test.followup"; id_of "test.root_mixed" ] settled;
  (* Observers have no State or Service authority; the test reads the
     Service afterwards. Both Effects ran before the root settled: *)
  eq ~pp:Format.pp_print_int "handled effects" 2 (Store.services f.store).handled_effects

(* Pushes its name on every Action start. *)
let named name calls =
  { Observe.none with action_start = (fun _ _ -> calls := !calls @ [ name ]) }

let additive_observers_run_in_order_and_clear_removes_them () =
  let f = make_store () in
  let calls = ref [] in
  Store.add_observer f.store (named "first" calls);
  Store.add_observer f.store (named "second" calls);
  dispatch f Followup;
  eq ~pp:pp_strings "both, in order" [ "first"; "second" ] !calls;

  calls := [];
  Store.clear_observers f.store;
  Store.add_observer f.store (named "replacement" calls);
  dispatch f Followup;
  eq ~pp:pp_strings "the replacement" [ "replacement" ] !calls;

  calls := [];
  Store.clear_observers f.store;
  dispatch f Followup;
  eq ~pp:pp_strings "none" [] !calls

let () =
  Alcotest.run "scheduling_order"
    [
      ( "scheduling_order",
        [
          Alcotest.test_case "typed dispatcher preserves one mixed FIFO" `Quick
            typed_dispatcher_preserves_one_mixed_fifo;
          Alcotest.test_case "typed items receive fresh globally linked metadata" `Quick
            typed_items_receive_fresh_globally_linked_metadata;
          Alcotest.test_case "pooled dispatcher resets each root emission sequence" `Quick
            pooled_dispatcher_resets_each_root_emission_sequence;
          Alcotest.test_case
            "effect-dispatched action completes before outer FIFO resumes" `Quick
            effect_dispatched_action_completes_before_outer_fifo_resumes;
          Alcotest.test_case
            "component output fanout retains one emission and lowering order" `Quick
            component_output_fanout_retains_one_emission_and_lowering_order;
          Alcotest.test_case "drain: disabled item keeps its reserved emission gap" `Quick
            drain_disabled_item_keeps_its_reserved_emission_gap;
          Alcotest.test_case "child action starts a fresh causal emission sequence" `Quick
            child_action_starts_a_fresh_causal_emission_sequence;
          Alcotest.test_case "disabled action invokes no lifecycle observer" `Quick
            disabled_action_invokes_no_lifecycle_observer;
        ] );
      ( "observers",
        [
          Alcotest.test_case "lifecycle observers wrap only their typed handlers" `Quick
            lifecycle_observers_wrap_only_their_typed_handlers;
          Alcotest.test_case "action_settled runs after the entire queued FIFO" `Quick
            action_settled_observer_runs_after_the_entire_queued_fifo;
          Alcotest.test_case "additive observers run in order and clear removes them"
            `Quick additive_observers_run_in_order_and_clear_removes_them;
        ] );
    ]
