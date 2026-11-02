(* Ported from the Rust port's [tests/harness.rs]: one component tested on
   its own. [Harness] drives its reducer with local State and records what
   it emits, with no Protocol or Store. The first tests run a child
   component's steps (Action, Effect, Output) in order; the later ones,
   child calls and requests; the last ones, one transition as a Store runs
   it. *)

open Causet

(* An emission in the Rust port's Debug form. *)
let show_emission (type a e o) (am : a Message.t) (em : e Message.t) (om : o Message.t)
    (emission : (a, e, o) emission) =
  match emission with
  | Emit_action a -> Format.asprintf "Action(%a)" am.pp a
  | Emit_effect e -> Format.asprintf "Effect(%a)" em.pp e
  | Emit_output o -> Format.asprintf "Output(%a)" om.pp o
  | Inline_child c -> Printf.sprintf "InlineChild(%s(%s))" c.slot c.call_kind
  | To_child c -> Printf.sprintf "ToChild(%s(%s))" c.slot c.call_kind

let show_emissions am em om l =
  "[" ^ String.concat ", " (List.map (show_emission am em om) l) ^ "]"

(* ------------------------------------------------------------------------ *)
(* A child component's protocol: child_action / child_effect / child_output *)

type child_action =
  | First
  | Last
  | Emit_all  (** Emits First, Request, Completed, Last. *)
  | Mutate_then_emit  (** Adds 1, checks the State, then emits First. *)
  | Emit_with_view  (** Adds 1, then emits Completed while reading the State. *)

type child_effect = Request
type child_output = Completed

let child_actions =
  Message.make ~name:"child.action"
    ~kind:(function
      | First -> "child.first"
      | Last -> "child.last"
      | Emit_all -> "child.emit_all"
      | Mutate_then_emit -> "child.mutate_then_emit"
      | Emit_with_view -> "child.emit_with_view")
    ~pp:(fun f a ->
      Format.pp_print_string f
        (match a with
        | First -> "First"
        | Last -> "Last"
        | Emit_all -> "EmitAll"
        | Mutate_then_emit -> "MutateThenEmit"
        | Emit_with_view -> "EmitWithView"))
    ()

let child_effects =
  Message.make ~name:"child.effect"
    ~kind:(fun Request -> "child.request")
    ~pp:(fun f Request -> Format.pp_print_string f "Request")
    ()

let child_outputs =
  Message.make ~name:"child.output"
    ~kind:(fun Completed -> "child.completed")
    ~pp:(fun f Completed -> Format.pp_print_string f "Completed")
    ()

let child_component : (int, unit, child_action, child_effect, child_output) Component.t =
  Component.make ~name:"child" ~actions:child_actions ~effects:child_effects
    ~outputs:child_outputs
    ~reduce:(fun ctx action _ ->
      match action with
      | Emit_all ->
          Ctx.emit_action ctx First;
          Ctx.emit_effect ctx Request;
          Ctx.emit_output ctx Completed;
          Ctx.emit_action ctx Last
      | Mutate_then_emit ->
          Ctx.update ctx (( + ) 1);
          if Ctx.state ctx <> 2 then failwith "assertion failed: the State should be 2";
          Ctx.emit_action ctx First
      | Emit_with_view ->
          Ctx.update ctx (( + ) 1);
          (* Rust's [into_emitter_with_view]: the State stays readable while
             emitting. *)
          let state = Ctx.state ctx in
          if state <> 2 then failwith "assertion failed: the view should be 2";
          Ctx.emit_output ctx Completed;
          if Ctx.state ctx <> 2 then
            failwith "assertion failed: the view should still be 2"
      | First | Last -> ())
    ~handle:(fun _ _ _ -> ())
    ()

let show_child = show_emissions child_actions child_effects child_outputs

let recording_emitter_preserves_one_ordered_stream () =
  let harness = Harness.create () in
  let _, emissions = Harness.reduce harness child_component 0 Emit_all in
  Alcotest.(check int) "four" 4 (List.length emissions);
  Alcotest.(check string)
    "emissions" "[Action(First), Effect(Request), Output(Completed), Action(Last)]"
    (show_child emissions)

let reducer_context_transitions_from_mutation_to_emission () =
  let harness = Harness.create () in
  let state, emissions = Harness.reduce harness child_component 1 Mutate_then_emit in
  Alcotest.(check int) "state" 2 state;
  Alcotest.(check string) "emissions" "[Action(First)]" (show_child emissions)

let reducer_context_can_emit_with_immutable_state_access () =
  let harness = Harness.create () in
  let _, emissions = Harness.reduce harness child_component 1 Emit_with_view in
  Alcotest.(check string) "emissions" "[Output(Completed)]" (show_child emissions)

(* ------------------------------------------------------------------------ *)
(* Child calls and requests under the harness *)

let kid_slot = Child.make "kid" child_component

type parent_state = { kid : int; requests : Request_id.t list; outcomes : outcome list }
type parent_action = Go | Done

let parent_actions =
  Message.make ~name:"parent.action"
    ~kind:(function Go -> "parent.go" | Done -> "parent.done")
    ~pp:(fun f a -> Format.pp_print_string f (match a with Go -> "Go" | Done -> "Done"))
    ()

(* Like the Rust one, it registers no child: under a Harness, child calls are
   recorded, never resolved. *)
let parent : (parent_state, unit, parent_action, never, never) Component.t =
  Component.make ~name:"parent" ~actions:parent_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      if action = Go then (
        let first = Ctx.new_request ctx in
        Ctx.update ctx (fun s -> { s with requests = s.requests @ [ first ] });
        let outcome = Ctx.reduce_child ctx kid_slot First in
        Ctx.update ctx (fun s -> { s with outcomes = s.outcomes @ [ outcome ] });
        let second = Ctx.new_request ctx in
        Ctx.update ctx (fun s -> { s with requests = s.requests @ [ second ] });
        Ctx.to_child ctx kid_slot Last;
        Ctx.emit_action ctx Done))
    ()

let empty_parent = { kid = 0; requests = []; outcomes = [] }

let child_calls_are_recorded_in_order_not_run () =
  let harness = Harness.create () in
  let state, emissions = Harness.reduce harness parent empty_parent Go in
  Alcotest.(check string)
    "emissions" "[InlineChild(kid(child.first)), ToChild(kid(child.last)), Action(Done)]"
    (show_emissions parent_actions Message.never Message.never emissions);
  (match emissions with
  | Inline_child call :: _ ->
      Alcotest.(check bool)
        "the call's action" true
        (Message.get child_actions call.call = Some First)
  | _ -> Alcotest.fail "an inline child call first");
  Alcotest.(check bool) "outcomes" true (state.outcomes = [ Reduced ]);
  Alcotest.(check int) "the child is recorded, not run" 0 state.kid

let requests_count_per_execution () =
  let harness = Harness.create () in
  let state, _ = Harness.reduce harness parent empty_parent Go in
  let state, _ = Harness.reduce harness parent state Go in
  let indexes =
    Array.of_list
      (List.map (fun (r : Request_id.t) -> (r.execution, r.index)) state.requests)
  in
  Alcotest.(check int) "first index" 0 (snd indexes.(0));
  Alcotest.(check int) "second index" 1 (snd indexes.(1));
  Alcotest.(check int) "a new execution restarts the counter" 0 (snd indexes.(2));
  Alcotest.(check bool) "distinct executions" true (fst indexes.(0) <> fst indexes.(2))

let a_component_with_effects_and_a_service_needs_no_store () =
  let harness = Harness.create () in
  let state, emissions =
    Harness.reduce harness Common.downloader Common.empty_downloader
      (Common.Fetch "alpha")
  in
  (match emissions with
  | [ Emit_effect (Common.Fetch_effect { request; url }) ] ->
      Alcotest.(check string) "url" "alpha" url;
      Alcotest.(check (option string))
        "pending" (Some "alpha")
        (List.assoc_opt request state.Common.pending)
  | l ->
      Alcotest.failf "expected one fetch Effect, got %s"
        (show_emissions Common.downloader_actions Common.downloader_effects Message.never
           l));
  Alcotest.(check bool)
    "probe enabled" true
    (Harness.is_enabled harness Common.downloader state Common.Probe);
  Alcotest.(check bool)
    "note 0 refused" false
    (Harness.is_enabled harness Common.downloader state (Common.Note 0))

(* ------------------------------------------------------------------------ *)
(* One transition as a Store runs it: admission at a chosen time, the new
   State, the ordered emissions, refusals as easy to test as successes. *)

(* Pings at most once per 10 time units, each one a request. *)
type pinger_state = { last : timestamp option; sent : Request_id.t list }
type ping = Ping
type send = Send of Request_id.t

let pings = Message.make ~name:"pinger.action" ~kind:(fun Ping -> "pinger.ping") ()
let sends = Message.make ~name:"pinger.effect" ~kind:(fun (Send _) -> "pinger.send") ()

let pinger : (pinger_state, unit, ping, send, never) Component.t =
  Component.make ~name:"pinger" ~actions:pings ~effects:sends ~outputs:Message.never
    ~is_enabled:(fun state Ping now ->
      match state.last with None -> true | Some last -> now >= last + 10)
    ~reduce:(fun ctx Ping (meta : Meta.t) ->
      let request = Ctx.new_request ctx in
      Ctx.update ctx (fun s -> { last = Some meta.time; sent = s.sent @ [ request ] });
      Ctx.emit_effect ctx (Send request))
    ~handle:(fun _ _ _ -> ())
    ()

let empty_pinger = { last = None; sent = [] }

let a_transition_is_stamped_from_the_time_given_in_the_session_given () =
  let harness = Harness.create ~session:7 () in
  Harness.at harness 1_000;
  match Harness.dispatch harness pinger empty_pinger Ping with
  | Error `Disabled -> Alcotest.fail "refused"
  | Ok (state, emissions) ->
      let request = { Request_id.session = 7; execution = 1; index = 0 } in
      (match emissions with
      | [ Emit_effect (Send r) ] when Request_id.equal r request -> ()
      | [ Emit_effect (Send r) ] ->
          Alcotest.failf "expected %a, got %a" Request_id.pp request Request_id.pp r
      | _ -> Alcotest.fail "expected one Send Effect");
      Alcotest.(check (option int)) "last" (Some 1_000) state.last

let a_refusal_leaves_state_and_time_alone () =
  let harness = Harness.create () in
  Harness.at harness 100;
  let state =
    match Harness.dispatch harness pinger empty_pinger Ping with
    | Ok (s, _) -> s
    | Error `Disabled -> Alcotest.fail "the first ping is refused"
  in
  (* Too soon: refused, and the refused attempt uses no time. *)
  Harness.at harness 105;
  Alcotest.(check bool)
    "refused" true
    (match Harness.dispatch harness pinger state Ping with
    | Error `Disabled -> true
    | Ok _ -> false);
  Alcotest.(check (triple (option int) int int))
    "unchanged" (Some 100, 1, 105)
    (state.last, List.length state.sent, Harness.next_time harness);
  Harness.at harness 110;
  match Harness.dispatch harness pinger state Ping with
  | Ok (state, _) -> Alcotest.(check (option int)) "last" (Some 110) state.last
  | Error `Disabled -> Alcotest.fail "the third ping is refused"

let times_never_go_back_when_the_clock_does () =
  let open Causet_guide.Shop in
  let harness = Harness.create () in
  let state = { items = []; total = 0 } in
  Harness.at harness 50;
  Alcotest.(check bool)
    "an empty cart" true
    (match Harness.dispatch harness cart state Checkout with
    | Error `Disabled -> true
    | Ok _ -> false);
  let state =
    match Harness.dispatch harness cart state (Add { item = "tea"; price = 4 }) with
    | Ok (s, _) -> s
    | Error `Disabled -> Alcotest.fail "the add is refused"
  in
  Alcotest.(check int) "next" 50 (Harness.next_time harness);
  Harness.at harness 10;
  Alcotest.(check int) "next after stepping back" 50 (Harness.next_time harness);
  Alcotest.(check bool)
    "checkout" true
    (Result.is_ok (Harness.dispatch harness cart state Checkout))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "harness"
    [
      ( "steps",
        [
          t "recording emitter preserves one ordered stream"
            recording_emitter_preserves_one_ordered_stream;
          t "reducer context transitions from mutation to emission"
            reducer_context_transitions_from_mutation_to_emission;
          t "reducer context can emit with immutable state access"
            reducer_context_can_emit_with_immutable_state_access;
        ] );
      ( "child calls and requests",
        [
          t "child calls are recorded in order, not run"
            child_calls_are_recorded_in_order_not_run;
          t "requests count per execution" requests_count_per_execution;
          t "a component with effects and a service needs no store"
            a_component_with_effects_and_a_service_needs_no_store;
        ] );
      ( "transitions",
        [
          t "a transition is stamped from the time given in the session given"
            a_transition_is_stamped_from_the_time_given_in_the_session_given;
          t "a refusal leaves state and time alone" a_refusal_leaves_state_and_time_alone;
          t "times never go back when the clock does"
            times_never_go_back_when_the_clock_does;
        ] );
    ]
