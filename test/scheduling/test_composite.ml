(* Port of the Rust tests/composite.rs.

   A leaf with no Effects, a composite holding it, and a root mounting the
   composite twice: [/primary] and [/secondary], each with its own [leaf]. *)

open Causet
open Common

(* ------------------------------------------------------------------------ *)
(* Leaf: no Effects, reports one Output. *)

type leaf_state = { value : int; leaf_requests : Request_id.t list }
type leaf_action = Bump of int | Request | Panic
type leaf_output = Bumped of { value : int }

let leaf_actions =
  Message.make ~name:"leaf.action"
    ~kind:(function
      | Bump _ -> "leaf.bump" | Request -> "leaf.request" | Panic -> "leaf.panic")
    ()

let leaf_outputs =
  Message.make ~name:"leaf.output" ~kind:(fun (Bumped _) -> "leaf.bumped") ()

let leaf : (leaf_state, unit, leaf_action, never, leaf_output) Component.t =
  Component.make ~name:"leaf" ~actions:leaf_actions ~effects:Message.never
    ~outputs:leaf_outputs
    ~reduce:(fun ctx action _ ->
      match action with
      | Bump n ->
          Ctx.update ctx (fun s -> { s with value = s.value + n });
          Ctx.emit_output ctx (Bumped { value = (Ctx.state ctx).value })
      | Request ->
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s ->
              { s with leaf_requests = s.leaf_requests @ [ request ] })
      | Panic -> failwith "leaf panicked")
    ()

let empty_leaf = { value = 0; leaf_requests = [] }

(* ------------------------------------------------------------------------ *)
(* Composite: holds a Leaf, calls it inline or queued, lowers its Outputs. *)

type composite_state = {
  leaf : leaf_state;
  requests : Request_id.t list;
  after_child : bool;
  seen : int list;
}

type composite_action =
  | Work
  | Bump_inline of int
  | Bump_queued of int
  | Leaf_bumped of int
  | Panic_in_child
  | Use_ghost

type composite_output = Reported of { value : int }

let composite_actions =
  Message.make ~name:"composite.action"
    ~kind:(function
      | Work -> "composite.work"
      | Bump_inline _ -> "composite.bump_inline"
      | Bump_queued _ -> "composite.bump_queued"
      | Leaf_bumped _ -> "composite.leaf_bumped"
      | Panic_in_child -> "composite.panic_in_child"
      | Use_ghost -> "composite.use_ghost")
    ()

let composite_outputs =
  Message.make ~name:"composite.output"
    ~kind:(fun (Reported _) -> "composite.reported")
    ()

let leaf_slot = Child.make "leaf" leaf
let ghost_slot = Child.make "ghost" leaf (* never bound *)

let composite :
    (composite_state, unit, composite_action, never, composite_output) Component.t =
  Component.make ~name:"composite" ~actions:composite_actions ~effects:Message.never
    ~outputs:composite_outputs
    ~reduce:(fun ctx action _ ->
      match action with
      | Work ->
          let first = Ctx.new_request ctx in
          Ctx.update ctx (fun s -> { s with requests = s.requests @ [ first ] });
          ignore (Ctx.reduce_child ctx leaf_slot Request);
          let last = Ctx.new_request ctx in
          Ctx.update ctx (fun s -> { s with requests = s.requests @ [ last ] })
      | Bump_inline n ->
          ignore (Ctx.reduce_child ctx leaf_slot (Bump n));
          Ctx.update ctx (fun s -> { s with after_child = true })
      | Bump_queued n -> Ctx.to_child ctx leaf_slot (Bump n)
      | Leaf_bumped value ->
          Ctx.update ctx (fun s -> { s with seen = s.seen @ [ value ] });
          Ctx.emit_output ctx (Reported { value })
      | Panic_in_child ->
          ignore (Ctx.reduce_child ctx leaf_slot Panic);
          Ctx.update ctx (fun s -> { s with after_child = true })
      | Use_ghost -> ignore (Ctx.reduce_child ctx ghost_slot (Bump 1)))
    ~register:(fun r ->
      Registrar.child r leaf_slot
        ~lens:(Lens.field ~get:(fun s -> s.leaf) ~set:(fun s leaf -> { s with leaf }))
        ~on_output:(fun _ (Bumped { value }) port ->
          Port.emit_action port (Leaf_bumped value))
        ())
    ()

let empty_composite = { leaf = empty_leaf; requests = []; after_child = false; seen = [] }

(* ------------------------------------------------------------------------ *)
(* App: the root, mounting Composite twice. *)

type app_state = {
  primary : composite_state;
  secondary : composite_state;
  secondary_open : bool;
  reports : (string * int) list;
  inline_outcomes : outcome list;
}

type app_action =
  | App_reported of { slot : string; value : int }
  | Open_secondary
  | Work_secondary_inline
  | Work_secondary_queued

let app_actions =
  Message.make ~name:"app.action"
    ~kind:(function
      | App_reported _ -> "app.reported"
      | Open_secondary -> "app.open_secondary"
      | Work_secondary_inline -> "app.work_secondary_inline"
      | Work_secondary_queued -> "app.work_secondary_queued")
    ()

let primary_slot = Child.make "primary" composite
let secondary_slot = Child.make "secondary" composite

(* One lowerer for both mounts: the slot says which one reported. *)
let lower_composite slot (Reported { value }) port =
  Port.emit_action port (App_reported { slot = Child.name slot; value })

let admit_secondary app _ _ = app.secondary_open

let app : (app_state, unit, app_action, never, never) Component.t =
  Component.make ~name:"app" ~actions:app_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | App_reported { slot; value } ->
          Ctx.update ctx (fun s -> { s with reports = s.reports @ [ (slot, value) ] })
      | Open_secondary -> Ctx.update ctx (fun s -> { s with secondary_open = true })
      | Work_secondary_inline ->
          let outcome = Ctx.reduce_child ctx secondary_slot Work in
          Ctx.update ctx (fun s ->
              { s with inline_outcomes = s.inline_outcomes @ [ outcome ] })
      | Work_secondary_queued -> Ctx.to_child ctx secondary_slot Work)
    ~register:(fun r ->
      Registrar.child r primary_slot
        ~lens:
          (Lens.field
             ~get:(fun s -> s.primary)
             ~set:(fun s primary -> { s with primary }))
        ~on_output:lower_composite ();
      Registrar.child r secondary_slot
        ~lens:
          (Lens.field
             ~get:(fun s -> s.secondary)
             ~set:(fun s secondary -> { s with secondary }))
        ~guard:admit_secondary ~on_output:lower_composite ())
    ()

let empty_app =
  {
    primary = empty_composite;
    secondary = empty_composite;
    secondary_open = false;
    reports = [];
    inline_outcomes = [];
  }

let protocol () =
  let reg = Registry.create () in
  Registry.root reg app;
  freeze_exn reg

let store p =
  let _sink, inbox = Event_bus.create p ~worker_capacity:16 in
  let store = Store.create_exn p ~state:empty_app ~services:() inbox in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  (store, log)

let root = Path.root app
let primary_path = Path.child root primary_slot
let secondary_path = Path.child root secondary_slot
let pp_strings = pp_list Format.pp_print_string
let pp_report f (slot, value) = Format.fprintf f "(%S, %d)" slot value

let pp_child_outcome f = function
  | Reduced -> Format.pp_print_string f "Reduced"
  | Refused -> Format.pp_print_string f "Refused"

let kinds log =
  List.map (fun (e : Observe.Trace_entry.t) -> e.kind) (Observe.Trace_log.entries log)

let ok name r = check_dispatch name (Ok Executed) r

(* ------------------------------------------------------------------------ *)

let composite_mounted_twice_gets_distinct_paths_and_ids () =
  let p = protocol () in
  let primary_leaf = Protocol.resolve_exn p (Path.child primary_path leaf_slot) in
  let secondary_leaf = Protocol.resolve_exn p (Path.child secondary_path leaf_slot) in
  eq ~pp:Format.pp_print_string "primary leaf path" "/primary/leaf"
    (Protocol.path_of p primary_leaf);
  eq ~pp:Format.pp_print_string "secondary leaf path" "/secondary/leaf"
    (Protocol.path_of p secondary_leaf);
  Alcotest.(check bool)
    "distinct instances" true
    (Protocol.instance_of primary_leaf <> Protocol.instance_of secondary_leaf);
  (* xxh3-64, seed 0, of the path: a fixed value that recordings depend on. *)
  eq ~pp:pp_instance "instance of /primary/leaf" 0xdb38_8269_9235_059dL
    (Protocol.instance_of primary_leaf)

let leaf_without_effects_reports_through_both_lowerers () =
  let p = protocol () in
  let s, log = store p in
  let leaf = Protocol.resolve_exn p (Path.child primary_path leaf_slot) in
  let composite = Protocol.resolve_exn p primary_path in
  ok "bump" (Store.dispatch_to s leaf (Bump 3));
  let st = Store.state s in
  eq ~pp:Format.pp_print_int "primary leaf" 3 st.primary.leaf.value;
  eq ~pp:Format.pp_print_int "secondary leaf" 0 st.secondary.leaf.value;
  eq ~pp:(pp_list Format.pp_print_int) "primary seen" [ 3 ] st.primary.seen;
  eq ~pp:(pp_list pp_report) "reports" [ ("primary", 3) ] st.reports;
  let entries = Observe.Trace_log.entries log in
  eq ~pp:pp_strings "paths and kinds"
    [ "/primary/leaf leaf.bump"; "/primary composite.leaf_bumped"; "/ app.reported" ]
    (List.map (fun (e : Observe.Trace_entry.t) -> e.path ^ " " ^ e.kind) entries);
  (* The lowered Action keeps the leaf as provenance, with the Output's
     emission ordinal and its own lowering ordinal. *)
  let nth n = List.nth entries n in
  let lowered = Option.get (nth 1).origin in
  eq ~pp:Format.pp_print_int "lowered causal" (nth 0).id lowered.causal;
  eq ~pp:pp_instance "lowered component" (Protocol.instance_of leaf) lowered.component;
  eq
    ~pp:(fun f (a, b) -> Format.fprintf f "(%d, %d)" a b)
    "ordinals" (0, 0)
    (lowered.emission_ordinal, lowered.lowering_ordinal);
  let reported = Option.get (nth 2).origin in
  eq ~pp:pp_instance "reported component"
    (Protocol.instance_of composite)
    reported.component

let parent_and_inline_child_share_one_request_counter () =
  let p = protocol () in
  let s, log = store p in
  let primary = Protocol.resolve_exn p primary_path in
  ok "work" (Store.dispatch_to s primary Work);
  let parent = (Store.state s).primary.requests in
  let child = (Store.state s).primary.leaf.leaf_requests in
  eq ~pp:Format.pp_print_int "parent requests" 2 (List.length parent);
  eq ~pp:Format.pp_print_int "child requests" 1 (List.length child);
  (* One execution: the parent's first request, the child's, the parent's last. *)
  let execution = (List.hd parent).execution in
  Alcotest.(check bool)
    "one execution, this session" true
    (List.for_all
       (fun (r : Request_id.t) -> r.execution = execution && r.session = Store.session s)
       (parent @ child));
  eq
    ~pp:(fun f (a, b, c) -> Format.fprintf f "(%d, %d, %d)" a b c)
    "indices" (0, 1, 2)
    ((List.nth parent 0).index, (List.hd child).index, (List.nth parent 1).index);
  (* The inline call added no execution of its own. *)
  eq ~pp:Format.pp_print_int "one execution" 1
    (List.length (Observe.Trace_log.entries log))

let inline_and_queued_calls_differ_in_executions () =
  let p = protocol () in
  let s, log = store p in
  let primary = Protocol.resolve_exn p primary_path in
  ok "inline" (Store.dispatch_to s primary (Bump_inline 2));
  eq ~pp:pp_strings "inline"
    [ "composite.bump_inline"; "composite.leaf_bumped"; "app.reported" ]
    (kinds log);
  Alcotest.(check bool) "after child" true (Store.state s).primary.after_child;
  ok "queued" (Store.dispatch_to s primary (Bump_queued 5));
  eq ~pp:pp_strings "queued"
    [ "composite.bump_queued"; "leaf.bump"; "composite.leaf_bumped"; "app.reported" ]
    (List.filteri (fun i _ -> i >= 3) (kinds log));
  eq ~pp:Format.pp_print_int "leaf value" 7 (Store.state s).primary.leaf.value

let one_lowerer_serves_both_mounts () =
  let p = protocol () in
  let s, _ = store p in
  let app_m = Protocol.resolve_exn p root in
  let primary = Protocol.resolve_exn p primary_path in
  let secondary = Protocol.resolve_exn p secondary_path in
  ok "open" (Store.dispatch_to s app_m Open_secondary);
  ok "primary" (Store.dispatch_to s primary (Bump_inline 1));
  ok "secondary" (Store.dispatch_to s secondary (Bump_inline 4));
  eq ~pp:(pp_list pp_report) "reports"
    [ ("primary", 1); ("secondary", 4) ]
    (Store.state s).reports;
  eq
    ~pp:(fun f (a, b) -> Format.fprintf f "(%d, %d)" a b)
    "leaf values" (1, 4)
    ((Store.state s).primary.leaf.value, (Store.state s).secondary.leaf.value)

let parent_guard_refuses_every_route () =
  let p = protocol () in
  let s, log = store p in
  let app_m = Protocol.resolve_exn p root in
  let secondary = Protocol.resolve_exn p secondary_path in
  (* External dispatch. *)
  check_dispatch "external" (Ok Disabled) (Store.dispatch_to s secondary Work);
  (* Inline call from the parent. *)
  ok "inline" (Store.dispatch_to s app_m Work_secondary_inline);
  eq ~pp:(pp_list pp_child_outcome) "refused inline" [ Refused ]
    (Store.state s).inline_outcomes;
  (* Queued send from the parent: refused at dequeue, so no execution. *)
  ok "queued" (Store.dispatch_to s app_m Work_secondary_queued);
  Alcotest.(check bool)
    "no execution at /secondary" true
    (List.for_all
       (fun (e : Observe.Trace_entry.t) -> e.path <> "/secondary")
       (Observe.Trace_log.entries log));
  eq ~pp:Format.pp_print_int "no requests" 0
    (List.length (Store.state s).secondary.requests);

  ok "open" (Store.dispatch_to s app_m Open_secondary);
  ok "inline again" (Store.dispatch_to s app_m Work_secondary_inline);
  eq ~pp:(pp_list pp_child_outcome) "outcomes" [ Refused; Reduced ]
    (Store.state s).inline_outcomes;
  eq ~pp:Format.pp_print_int "requests" 2 (List.length (Store.state s).secondary.requests)

let an_inline_call_makes_no_execution_of_its_own () =
  let p = protocol () in
  let s, log = store p in
  let primary = Protocol.resolve_exn p primary_path in
  ok "inline" (Store.dispatch_to s primary (Bump_inline 1));
  let entries = Observe.Trace_log.entries log in
  eq ~pp:pp_strings "executed"
    [
      "/primary composite.bump_inline"; "/primary composite.leaf_bumped"; "/ app.reported";
    ]
    (List.map (fun (e : Observe.Trace_entry.t) -> e.path ^ " " ^ e.kind) entries);
  (* The leaf's Action ran inside the composite's execution. *)
  eq ~pp:pp_strings "inline"
    [ Printf.sprintf "/primary/leaf leaf.bump within %d" (List.hd entries).id ]
    (List.map
       (fun (i : Observe.Inline_entry.t) ->
         Printf.sprintf "%s %s within %d" i.path i.kind i.within)
       (Observe.Trace_log.inline log))

let panic_in_inline_child_faults_and_parent_never_resumes () =
  let p = protocol () in
  let s, _ = store p in
  let primary = Protocol.resolve_exn p primary_path in
  check_dispatch "faulted" (Error (Faulted "leaf panicked"))
    (Store.dispatch_to s primary Panic_in_child);
  Alcotest.(check bool) "parent never resumed" false (Store.state s).primary.after_child;
  check_dispatch "still faulted" (Error (Faulted "leaf panicked"))
    (Store.dispatch_to s primary Work)

let unbound_slot_faults_at_the_call () =
  let p = protocol () in
  let s, _ = store p in
  let primary = Protocol.resolve_exn p primary_path in
  match Store.dispatch_to s primary Use_ghost with
  | Error (Faulted message) ->
      if not (contains ~sub:"slot ghost is not bound under /primary" message) then
        Alcotest.failf "unexpected message: %s" message
  | _ -> Alcotest.fail "expected a fault"

let mount_from_another_protocol_is_rejected () =
  let first = protocol () in
  let second = protocol () in
  let s, _ = store second in
  let foreign = Protocol.resolve_exn first primary_path in
  check_dispatch "foreign" (Error Foreign_mount) (Store.dispatch_to s foreign Work);
  (* The same Protocol serves several Stores. *)
  let other, _ = store first in
  check_dispatch "own" (Ok Executed) (Store.dispatch_to other foreign Work)

let freeze_rejects_an_unlowered_output_and_a_reporting_root () =
  let c_slot = Child.make "c" composite in
  let bare : (app_state, unit, app_action, never, never) Component.t =
    Component.make ~name:"bare" ~actions:app_actions ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun _ _ _ -> ())
      ~register:(fun r ->
        (* Composite reports Outputs, but no lowerer is bound. *)
        Registrar.child r c_slot
          ~lens:
            (Lens.field
               ~get:(fun s -> s.primary)
               ~set:(fun s primary -> { s with primary }))
          ())
      ()
  in
  let reg = Registry.create () in
  Registry.root reg bare;
  (match (Registry.freeze reg : ((app_state, unit) Protocol.t, problem list) result) with
  | Ok _ -> Alcotest.fail "must fail"
  | Error problems ->
      if not (List.mem (No_lowerer "/c") problems) then
        Alcotest.failf "no No_lowerer /c in: %s"
          (String.concat "; " (List.map problem_message problems)));

  let reporting :
      (composite_state, unit, composite_action, never, composite_output) Component.t =
    Component.make ~name:"reporting" ~actions:composite_actions ~effects:Message.never
      ~outputs:composite_outputs
      ~reduce:(fun _ _ _ -> ())
      ()
  in
  let reg : (composite_state, unit) Registry.t = Registry.create () in
  Registry.root reg reporting;
  Alcotest.(check bool) "a reporting root" true (Result.is_error (Registry.freeze reg))

let () =
  Alcotest.run "composite"
    [
      ( "composite",
        [
          Alcotest.test_case "composite mounted twice gets distinct paths and ids" `Quick
            composite_mounted_twice_gets_distinct_paths_and_ids;
          Alcotest.test_case "leaf without effects reports through both lowerers" `Quick
            leaf_without_effects_reports_through_both_lowerers;
          Alcotest.test_case "parent and inline child share one request counter" `Quick
            parent_and_inline_child_share_one_request_counter;
          Alcotest.test_case "inline and queued calls differ in executions" `Quick
            inline_and_queued_calls_differ_in_executions;
          Alcotest.test_case "one lowerer serves both mounts" `Quick
            one_lowerer_serves_both_mounts;
          Alcotest.test_case "parent guard refuses every route" `Quick
            parent_guard_refuses_every_route;
          Alcotest.test_case "an inline call makes no execution of its own" `Quick
            an_inline_call_makes_no_execution_of_its_own;
          Alcotest.test_case "panic in inline child faults and parent never resumes"
            `Quick panic_in_inline_child_faults_and_parent_never_resumes;
          Alcotest.test_case "unbound slot faults at the call" `Quick
            unbound_slot_faults_at_the_call;
          Alcotest.test_case "mount from another protocol is rejected" `Quick
            mount_from_another_protocol_is_rejected;
          Alcotest.test_case "freeze rejects an unlowered output and a reporting root"
            `Quick freeze_rejects_an_unlowered_output_and_a_reporting_root;
        ] );
    ]
