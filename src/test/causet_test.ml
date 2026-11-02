(* Testing and simulation on the real Store.

   - [Node]: a Store on a held bus, with a chosen session and a shared clock.
     Every Event a Service or a test sends is held until the test delivers
     it, and the application's invariants are checked after every root input.
   - [Scenario]: drives several nodes on one virtual clock. It delivers held
     Events (by default in the order the bus's lanes would run them),
     advances the clock to the next timer deadline or to the next time the
     application's [World] names, and stops at a predicate, a time or a step
     budget.
   - [Rng]: a seeded generator, for the choices a scenario or a simulated
     network makes; [Rng.split] gives the world a stream of its own.
   - [Swarm]: the step kinds a run uses, drawn once per run.
   - [In_flight]: a simulated world's messages in flight.
   - [check]: generated runs, drawn from one stream of choices, and a failing
     run shrunk to a small list of choices that [replay] repeats.

   A scenario repeats from its seed, its fixture and any explicit choices
   only when every Service in it is simulated. *)

open Causet

(* A run's failure, named: what [check] keys a failure by and shrinking keeps
   failing the same way. *)
exception Failed of { key : string; message : string }

(* A run the generator rejects: not counted as a run or a failure. *)
exception Rejected

let fail ~key message = raise (Failed { key; message })

(* Where a failure happened, to key it by. An exception raised by a tail
   call ([failwith] last in a reducer) leaves no frame of its caller, so its
   location is causet's own code, which runs every reducer: then the message
   tells failures apart too. *)
let place location message =
  match location with
  | None -> "an unknown place: " ^ message
  | Some at ->
      let file =
        match String.index_opt at ':' with Some i -> String.sub at 0 i | None -> at
      in
      if
        List.mem (Filename.basename file)
          [
            "kernel.ml";
            "kernel_types.ml";
            "kernel_readiness.ml";
            "kernel_observe.ml";
            "kernel_state.ml";
            "kernel_schedule.ml";
            "kernel_context.ml";
            "kernel_bus.ml";
            "kernel_registry.ml";
            "kernel_effect.ml";
            "kernel_store.ml";
            "kernel_harness.ml";
            "timer.ml";
            "bug.ml";
            "clock.ml";
            "causet_test.ml";
          ]
      then at ^ ": " ^ message
      else at

(* ------------------------------------------------------------------------ *)
(* Rng *)

(* The choices of a run. Every draw is a bounded integer, "3 out of 0..10",
   and 0 is always the simplest answer. A generator from a seed draws with
   SplitMix64; one that [check] or [replay] makes also keeps every choice, or
   replays a list of them, so a run is described completely by its list.
   Copies share one stream. *)
module Rng = struct
  type source =
    | Seeded of { seed : int; mutable state : int64 }
    | Replay of { choices : int array; mutable at : int }

  type t = { source : source; mutable kept : int list option (* newest first *) }

  let split_mix state =
    let open Int64 in
    let state = add state 0x9e3779b97f4a7c15L in
    let z = state in
    let z = mul (logxor z (shift_right_logical z 30)) 0xbf58476d1ce4e5b9L in
    let z = mul (logxor z (shift_right_logical z 27)) 0x94d049bb133111ebL in
    (state, logxor z (shift_right_logical z 31))

  (* A generator starting from [seed]. *)
  let create seed = { source = Seeded { seed; state = Int64.of_int seed }; kept = None }
  let recording seed = { (create seed) with kept = Some [] }

  let replaying choices =
    { source = Replay { choices = Array.of_list choices; at = 0 }; kept = Some [] }

  (* The seed, for a generator drawing from one. *)
  let seed r = match r.source with Seeded { seed; _ } -> Some seed | Replay _ -> None

  (* The choices made so far, if this generator keeps them. *)
  let choices r = Option.map List.rev r.kept

  (* A number in [0, n); 0, and no choice made, when [n] is 0 or 1. *)
  let below r n =
    if n <= 1 then 0
    else
      let choice =
        match r.source with
        | Seeded s ->
            let state, z = split_mix s.state in
            s.state <- state;
            Int64.to_int (Int64.unsigned_rem z (Int64.of_int n))
        | Replay p ->
            let c =
              if p.at < Array.length p.choices then min p.choices.(p.at) (n - 1) else 0
            in
            p.at <- p.at + 1;
            c
      in
      (match r.kept with Some k -> r.kept <- Some (choice :: k) | None -> ());
      choice

  (* A number below 2^53 - 1, as one choice. Every port draws it below the
     same bound, the largest TypeScript holds exactly, so a seed's stream,
     and every world [split] seeds from it, is the same in every port. *)
  let next r = below r ((1 lsl 53) - 1)

  (* A generator for the run's world: one choice, a seed, and a generator
     seeded from it that records nothing. *)
  let split r = create (next r)

  (* A number in [low, high]. *)
  let between r low high = low + below r (high - low + 1)

  (* True with probability [num / den]; a choice of 0 says no. *)
  let chance r num den = below r den >= den - num

  (* Whether a run takes another step: 15 times in 16. *)
  let more r = chance r 15 16

  (* One of [items], the first being the simplest. *)
  let pick r items =
    if items = [||] then invalid_arg "pick from no items";
    items.(below r (Array.length items))

  (* Ends this run as rejected (an invalid fixture, say). *)
  let reject _ = raise Rejected
end

(* ------------------------------------------------------------------------ *)
(* Swarm *)

(* The step kinds a run uses, drawn once at its start: each is on with chance
   1/2 (a choice of 0 meaning on); if none is on, all are. *)
module Swarm = struct
  type 'k t = { on : ('k * int) list; total : int }

  let create rng kinds =
    if kinds = [] then invalid_arg "a swarm of no kinds";
    if List.exists (fun (_, w) -> w <= 0) kinds then
      invalid_arg "a kind of weight 0 is never picked";
    let fits =
      List.fold_left
        (fun total (_, w) ->
          match total with Some t when t <= max_int - w -> Some (t + w) | _ -> None)
        (Some 0) kinds
    in
    if fits = None then invalid_arg "a swarm's weights must total at most max_int";
    let on = List.filter (fun _ -> Rng.below rng 2 = 0) kinds in
    let on = if on = [] then kinds else on in
    { on; total = List.fold_left (fun t (_, w) -> t + w) 0 on }

  (* One of the kinds on, by weight. *)
  let pick s rng =
    let rec go at = function
      | [] -> assert false
      | (k, w) :: rest -> if at < w then k else go (at - w) rest
    in
    go (Rng.below rng s.total) s.on

  let kinds s = List.map fst s.on
end

(* ------------------------------------------------------------------------ *)
(* What a run's thread knows about it *)

type run = {
  mutable steps : int;
  mutable max_steps : int option;
  mutable trace : string list option;
      (** While a run is checked for determinism: every node's executions, refusals and
          inline calls, newest first. *)
}

let run_state =
  Domain.DLS.new_key (fun () -> { steps = 0; max_steps = None; trace = None })

(* Records what a node's Store did, while a run's executions are kept. *)
let record line =
  let r = Domain.DLS.get run_state in
  match r.trace with Some l -> r.trace <- Some (line () :: l) | None -> ()

let refused_name : Observe.refused_by -> string = function
  | Root_guard -> "root-guard"
  | Guard -> "guard"
  | Component -> "component"
  | Absent -> "absent"

(* Records a node's executions, refusals and inline calls while a generated
   run is checked for determinism; nothing otherwise. Each line carries the
   message's printed form, so two runs that send the same kinds with other
   values differ. *)
let run_trace node =
  let execution (e : Observe.Execution.t) payload =
    record (fun () ->
        Format.asprintf "%s %s %s %s id %d time %d causal %d depth %d: %a" node
          (Meta.category_name e.category)
          e.path e.kind e.meta.id e.meta.time
          (Meta.causal_execution_id e.meta)
          e.meta.depth Message.pp_payload payload)
  in
  {
    Observe.none with
    action_start = execution;
    effect_start = execution;
    event_start = execution;
    action_refused =
      (fun (r : Observe.Refusal.t) payload ->
        record (fun () ->
            Format.asprintf "%s refused %s %s by %s at %d: %a" node r.path r.kind
              (refused_name r.by) r.time Message.pp_payload payload));
    inline_action =
      (fun (i : Observe.Inline_call.t) payload ->
        record (fun () ->
            Format.asprintf "%s inline %s %s within %d at %d: %a" node i.path i.kind
              i.within i.time Message.pp_payload payload));
  }

(* Counts a root input; past the run's step limit, fails the run. *)
let count_step node =
  let r = Domain.DLS.get run_state in
  r.steps <- r.steps + 1;
  match r.max_steps with
  | Some max when r.steps > max ->
      fail ~key:"step budget"
        (Printf.sprintf
           "node %s: the run took more than %d root inputs (config.max_steps)" node max)
  | _ -> ()

(* ------------------------------------------------------------------------ *)
(* Groups *)

(* Why a group's checks run. *)
type group_step = Added | Published of string | Settled of string | Restarted of string

(* A named group of nodes whose views shared checks read. All its members
   publish the same type of view. *)
module Group = struct
  type 'v t = {
    name : string;
    mutable views : (string * 'v) list;  (** by node name, in name order *)
    mutable checks :
      (string * ((string * 'v) list -> group_step -> (unit, string) result)) list;
  }

  let create name = { name; views = []; checks = [] }
  let name g = g.name

  let describe = function
    | Added -> "when the check was added"
    | Published n -> "when " ^ n ^ " published"
    | Settled n -> "after a root input of " ^ n
    | Restarted n -> "when " ^ n ^ " restarted"

  let run_checks g checks step =
    List.iter
      (fun (name, check) ->
        let result =
          try check g.views step
          with e ->
            fail
              ~key:(Printf.sprintf "group %s: invariant %S panicked" g.name name)
              (exn_message e)
        in
        match result with
        | Ok () -> ()
        | Error e ->
            fail
              ~key:(Printf.sprintf "group %s: invariant %S" g.name name)
              (Printf.sprintf "group %s, %s: invariant %S: %s" g.name (describe step) name
                 e))
      checks

  (* A shared invariant: what is wrong across the views, if anything. It runs
     now, then whenever a member publishes, settles a root input or
     restarts. Its captures are its memory. *)
  let check g name f =
    g.checks <- g.checks @ [ (name, f) ];
    run_checks g [ (name, f) ] Added

  let run g step = run_checks g g.checks step

  let update g node view =
    g.views <-
      List.sort
        (fun (a, _) (b, _) -> compare a b)
        ((node, view) :: List.remove_assoc node g.views)

  (* [node]'s view goes, without running checks. *)
  let leave g node = g.views <- List.remove_assoc node g.views

  let join g node view =
    if List.mem_assoc node g.views then
      invalid_arg (Printf.sprintf "group %s: node %S has already published" g.name node);
    update g node view;
    (* A failing check leaves no member behind. *)
    try run g (Published node)
    with e ->
      leave g node;
      raise e

  (* The view [node] published, if it has. *)
  let view views node = List.assoc_opt node views
end

(* ------------------------------------------------------------------------ *)
(* Node *)

(* Which of one bus's held Events (as [Held.list] gives them, in send
   order) its lanes would run next had none been held: the drain takes the
   Store lane before the worker lane, and each lane holds the Events sent
   to its head newest first, then the others oldest first. Delivering
   Events one at a time in this order, each with the lanes empty, runs them
   as production would. *)
let next_in_queue_order (held : held_event list) =
  let indexed = List.mapi (fun i e -> (i, e)) held in
  let in_lane lane =
    let newest_front =
      List.fold_left
        (fun acc (i, e) -> if e.held_lane = lane && e.held_front then Some i else acc)
        None indexed
    in
    match newest_front with
    | Some i -> Some i
    | None ->
        Option.map fst
          (List.find_opt (fun (_, e) -> e.held_lane = lane && not e.held_front) indexed)
  in
  match in_lane Store_lane with Some i -> Some i | None -> in_lane Worker_lane

(* How many Events a node's worker lane holds once released. *)
let worker_capacity = 1024

(* A real Store under test. Its functions fail the run, naming the node and
   the step, when the Store faults, an invariant fails or a bug is reported
   unexpectedly. *)
module Node = struct
  type ('s, 'svc) t = {
    name : string;
    proto : ('s, 'svc) Protocol.t;
    clock : Clock.Manual.m;
    mutable store : ('s, 'svc) Store.t;
    mutable sink : Sink.t;
    mutable held : Held.t;
    mutable bugs : Observe.Bug_log.log;
    mutable timers : Timer.Control.t option;
    mutable publications : ('s -> unit) list;  (** refresh each view *)
    mutable groups : (group_step -> unit) list;  (** run each group's checks *)
    mutable leaves : (unit -> unit) list;  (** leave each group *)
    mutable steps : int;
    mutable bugs_seen : int;
    mutable allow_bugs : bool;
  }

  let open_store name proto state services session clock =
    let sink, inbox, held = Event_bus.held proto ~worker_capacity session in
    let store =
      Store.create_exn ~clock:(Clock.Manual.clock clock) proto ~state ~services inbox
    in
    let bugs = Observe.Bug_log.create () in
    Store.add_observer store (Observe.Bug_log.observer bugs);
    Store.add_observer store (run_trace name);
    (store, sink, held, bugs)

  (* A node running [proto], starting from [state] with [services]. *)
  let of_protocol ~name proto ~state ~services ~session clock =
    let store, sink, held, bugs = open_store name proto state services session clock in
    {
      name;
      proto;
      clock;
      store;
      sink;
      held;
      bugs;
      timers = None;
      publications = [];
      groups = [];
      leaves = [];
      steps = 0;
      bugs_seen = 0;
      allow_bugs = false;
    }

  (* A node whose root component is [root]. *)
  let create ~name root ~state ~services ~session clock =
    let reg = Registry.create () in
    Registry.root reg root;
    match Registry.freeze reg with
    | Ok proto -> of_protocol ~name proto ~state ~services ~session clock
    | Error problems ->
        failwith
          (Printf.sprintf "node %s: %s" name
             (String.concat "; " (List.map problem_message problems)))

  (* Bug reports are expected: they no longer fail the node. *)
  let allow_bugs node =
    node.allow_bugs <- true;
    node

  let bug_reports node = Observe.Bug_log.reports node.bugs

  (* The node's simulated timer, so a scenario fires it as time passes. *)
  let with_timer node control =
    node.timers <- Some control;
    node

  let name node = node.name
  let state node = Store.state node.store
  let store node = node.store
  let sink node = node.sink
  let protocol node = node.proto
  let session node = Store.session node.store
  let mount node path = Protocol.resolve_exn node.proto path

  (* Fails on the first report not seen yet that fails the node: any
     invariant's, and any other unless [allow_bugs]. *)
  let assert_reports node when_ =
    let reports = bug_reports node in
    let fresh = List.filteri (fun i _ -> i >= node.bugs_seen) reports in
    node.bugs_seen <- List.length reports;
    List.iter
      (fun (r : Observe.Bug_report.t) ->
        let at = Printf.sprintf "%s:%d" r.file r.line in
        match Option.map (fun (p : Observe.Bug_place.t) -> p.phase) r.place with
        | Some (Observe.Invariant name) ->
            fail
              ~key:(Printf.sprintf "node %s: invariant %S" node.name name)
              (Printf.sprintf "node %s, %s: invariant %S: %s" node.name when_ name
                 r.message)
        | Some (Observe.Invariant_panic name) ->
            fail
              ~key:(Printf.sprintf "node %s: invariant %S panicked" node.name name)
              (Printf.sprintf "node %s, %s: invariant %S: %s" node.name when_ name
                 r.message)
        | _ when node.allow_bugs -> ()
        | _ ->
            fail
              ~key:(Printf.sprintf "node %s: bug report at %s" node.name at)
              (Printf.sprintf "node %s, %s: unexpected bug report at %s: %s" node.name
                 when_ at r.message))
      fresh

  (* An invariant, registered on the Store: checked at once, then after
     every root input; a violation fails the node. *)
  let check_with ?pos node name f =
    Store.add_invariant ?pos node.store name f;
    assert_reports node
      (if node.steps = 0 then "before any root input" else "when the check was added");
    node

  let check ?pos node name f = check_with ?pos node name (fun s _ -> f s)

  (* Publishes a view of this node's State into [group]: made now, then after
     each of the node's root inputs, for the group's checks to read. *)
  let publish node group view =
    Group.join group node.name (view (state node));
    node.publications <-
      node.publications @ [ (fun s -> Group.update group node.name (view s)) ];
    node.groups <- node.groups @ [ Group.run group ];
    node.leaves <- node.leaves @ [ (fun () -> Group.leave group node.name) ];
    node

  let refresh_groups node step =
    if node.publications <> [] then (
      let s = state node in
      List.iter (fun refresh -> refresh s) node.publications;
      List.iter (fun run -> run step) node.groups)

  let fault node e =
    let message =
      match e with
      | Faulted m -> m
      | Foreign_mount -> "foreign mount"
      | Foreign_bus -> "foreign bus"
    in
    fail
      ~key:
        (Printf.sprintf "node %s: fault at %s" node.name
           (place (Store.fault_location node.store) message))
      (Printf.sprintf "node %s, root input %d: the Store faulted: %s" node.name node.steps
         message)

  let settled node =
    assert_reports node (Printf.sprintf "after root input %d" node.steps);
    refresh_groups node (Settled node.name)

  (* Dispatches an Action as a root input, then checks the invariants. *)
  let dispatch node mount action =
    count_step node.name;
    let outcome = Store.dispatch_to node.store mount action in
    node.steps <- node.steps + 1;
    match outcome with
    | Error e -> fault node e
    | Ok Executed ->
        settled node;
        Executed
    | Ok Disabled ->
        assert_reports node (Printf.sprintf "after root input %d" node.steps);
        Disabled
    | Ok Faulted -> invalid_arg "causet: a root input returns its fault as an error"

  (* The Events held for this node, in send order. *)
  let held node = Held.list node.held
  let inspect node id msg = Held.inspect node.held id msg

  let drain node =
    let rec loop () =
      match Store.drain_events node.store 1 with
      | Ok 0 -> ()
      | Ok _ ->
          count_step node.name;
          node.steps <- node.steps + 1;
          settled node;
          loop ()
      | Error e ->
          node.steps <- node.steps + 1;
          fault node e
    in
    loop ()

  (* Delivers the held Event [id]: releases it, then runs every released
     Event as a root input, checking the invariants after each. *)
  let deliver node id =
    (match Held.release node.held id with
    | Ok () -> ()
    | Error _ ->
        invalid_arg (Printf.sprintf "node %s: cannot release held Event %d" node.name id));
    drain node

  (* Delivers the held Event the bus would run next, and returns it. *)
  let deliver_next node =
    let held = held node in
    match next_in_queue_order held with
    | None -> None
    | Some i ->
        let e = List.nth held i in
        deliver node e.held_id;
        Some e

  (* Delivers the first held Event of [msg]'s type for which [select] is
     true. Returns whether there was one. *)
  let deliver_where node msg select =
    match
      List.find_opt
        (fun e ->
          match inspect node e.held_id msg with Some v -> select v | None -> false)
        (held node)
    with
    | Some e ->
        deliver node e.held_id;
        true
    | None -> false

  (* Drops the held Event [id], as a lost message. *)
  let discard node id = Held.discard node.held id

  (* Fires the node's simulated timers that are due on the shared clock. *)
  let fire_timers node =
    match node.timers with Some t -> Timer.Control.fire_due t | None -> 0

  let next_deadline node = Option.bind node.timers Timer.Control.next_deadline

  (* The node is gone: its Store closes, and its views leave their groups,
     without running checks. *)
  let close node =
    Store.close node.store;
    List.iter (fun leave -> leave ()) node.leaves;
    node.leaves <- [];
    node.publications <- [];
    node.groups <- []

  (* The node restarted: its Store, bus, held Events and timers are gone,
     and a new Store starts from [state] with a new [session]. The protocol,
     the clock, the invariants with their memory and the node's groups stay;
     the invariants run first on the restored State, then the groups'
     checks. The new Services' simulated timer is given with
     [with_timer]. *)
  let restart node ~state ~services ~session =
    let invariants = Store.take_invariants node.store in
    Store.close node.store;
    let store, sink, held, bugs =
      open_store node.name node.proto state services session node.clock
    in
    node.store <- store;
    node.sink <- sink;
    node.held <- held;
    node.bugs <- bugs;
    node.bugs_seen <- 0;
    node.timers <- None;
    Store.restore_invariants store invariants;
    assert_reports node "on restart";
    refresh_groups node (Restarted node.name)
end

(* ------------------------------------------------------------------------ *)
(* Scenario *)

(* What a scenario can do with a node, whatever its State and Services. *)
type driven = {
  d_name : string;
  d_held : unit -> held_event list;
  d_deliver : int -> unit;
  d_next_deadline : unit -> int option;
  d_fire_timers : unit -> int;
}

let driven node =
  {
    d_name = Node.name node;
    d_held = (fun () -> Node.held node);
    d_deliver = Node.deliver node;
    d_next_deadline = (fun () -> Node.next_deadline node);
    d_fire_timers = (fun () -> Node.fire_timers node);
  }

(* The application's world outside the Stores, such as a simulated network:
   what it will do next, and doing it once the clock gets there. *)
type world = { next_time : unit -> int option; advance : int -> unit }

let no_world = { next_time = (fun () -> None); advance = ignore }

(* What a simulated world has in flight: messages due at a time, delivered in
   that order, and in the order they were sent when due together. On a
   channel, a message can also be kept after the one before it. *)
module In_flight = struct
  module Key = struct
    type t = int * int

    let compare = compare
  end

  module Q = Map.Make (Key)

  type ('m, 'c) t = {
    mutable queue : 'm Q.t;
    mutable sent : int;
    mutable last : ('c * int) list;
  }

  let create () = { queue = Q.empty; sent = 0; last = [] }

  (* [message], due at [at]. *)
  let send_at t at message =
    t.queue <- Q.add (at, t.sent) message t.queue;
    t.sent <- t.sent + 1

  (* [message] on [channel], due at [at] or just after the channel's last
     message, whichever is later. Returns when it is due. *)
  let send_in_order t channel at message =
    let at =
      match List.assoc_opt channel t.last with
      | Some last -> max at (if last = max_int then last else last + 1)
      | None -> at
    in
    t.last <- (channel, at) :: List.remove_assoc channel t.last;
    send_at t at message;
    at

  (* What is in flight, in the order it will be delivered. *)
  let to_list t = List.map (fun ((at, _), m) -> (at, m)) (Q.bindings t.queue)

  (* When the next message is due. *)
  let next_time t = Option.map (fun ((at, _), _) -> at) (Q.min_binding_opt t.queue)

  (* Every message due at [now] or before, in order, taken out. *)
  let due t now =
    let ready, later = Q.partition (fun (at, _) _ -> at <= now) t.queue in
    t.queue <- later;
    List.map snd (Q.bindings ready)

  let length t = Q.cardinal t.queue
  let is_empty t = Q.is_empty t.queue

  (* What it holds, in order, with when each is due. *)
  let pp pp_message f t =
    Format.fprintf f "@[<hv 2>In_flight [%a]@]"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
         (fun f (at, m) -> Format.fprintf f "%d: %a" at pp_message m))
      (to_list t)
end

(* One step of a scenario. *)
type step =
  | Delivered of { node : int; event : held_event }
  | Advanced of { to_ : int; fired : int }

(* Why a scenario stopped. *)
type stop = Reached | Quiet | Budget | Time_limit

(* Drives several nodes on one virtual clock. *)
module Scenario = struct
  type t = {
    clock : Clock.Manual.m;
    rng : Rng.t;
    mutable log : string list (* newest first *);
  }

  (* A scenario starting at [start] on its clock, with choices from [seed]. *)
  let create seed start =
    { clock = Clock.Manual.create start; rng = Rng.create seed; log = [] }

  (* A scenario drawing from [rng], a generated run's. *)
  let with_rng rng start = { clock = Clock.Manual.create start; rng; log = [] }
  let clock s = s.clock
  let rng s = s.rng
  let note s line = s.log <- line :: s.log
  let seed s = Rng.seed s.rng
  let log s = List.rev s.log

  (* The failure report: the seed, the step count and the last [last]
     steps. *)
  let report s last =
    let steps = log s in
    let n = List.length steps in
    let head =
      match seed s with
      | Some seed -> Printf.sprintf "scenario seed %d failed after %d steps" seed n
      | None -> Printf.sprintf "scenario on replayed choices failed after %d steps" n
    in
    let skipped = max 0 (n - last) in
    let head =
      if skipped > 0 then Printf.sprintf "%s; the last %d:" head last
      else if n > 0 then head ^ ":"
      else head
    in
    String.concat ""
      (head
      :: List.filteri
           (fun i _ -> i >= skipped)
           (List.mapi (fun i step -> Printf.sprintf "\n  %4d. %s" (i + 1) step) steps))

  let next_time nodes world =
    List.fold_left
      (fun acc t ->
        match (acc, t) with
        | None, t -> t
        | a, None -> a
        | Some a, Some b -> Some (min a b))
      (world.next_time ())
      (List.map (fun d -> d.d_next_deadline ()) nodes)

  (* [step], with [choose] picking which held Event to deliver among every
     node's: [(node, event)] pairs, node by node, each node's in send order.
     It returns an index into them. *)
  let step_with s nodes world ~choose =
    let candidates =
      List.concat (List.mapi (fun i d -> List.map (fun e -> (i, e)) (d.d_held ())) nodes)
    in
    if candidates <> [] then (
      let pick = min (choose candidates s.rng) (List.length candidates - 1) in
      let node, event = List.nth candidates pick in
      let d = List.nth nodes node in
      s.log <-
        Printf.sprintf "%s: deliver %s to %s" d.d_name event.held_kind event.held_path
        :: s.log;
      d.d_deliver event.held_id;
      Some (Delivered { node; event }))
    else
      match next_time nodes world with
      | None -> None
      | Some next ->
          let to_ = max next (Clock.Manual.get s.clock) in
          Clock.Manual.set s.clock to_;
          world.advance to_;
          let fired = List.fold_left (fun acc d -> acc + d.d_fire_timers ()) 0 nodes in
          s.log <- Printf.sprintf "advance to %d: %d timers fired" to_ fired :: s.log;
          Some (Advanced { to_; fired })

  (* One step: deliver a held Event if any node has one (of the first node
     that has one, the Event its lanes would run next), else advance the
     clock to the next timer deadline or world time, let the world act, then
     fire the timers due. [None] when nothing is held or ahead. *)
  let step s nodes world =
    step_with s nodes world ~choose:(fun candidates _ ->
        let node = fst (List.hd candidates) in
        let first = List.filter (fun (n, _) -> n = node) candidates |> List.map snd in
        match next_in_queue_order first with Some i -> i | None -> 0)

  (* Steps until [until] holds, nothing is left, [steps] steps were taken, or
     the next thing to do lies past [time_limit]. *)
  let run_until ?time_limit s nodes world ~steps ~until =
    let rec go n =
      if until () then Reached
      else if n = 0 then Budget
      else
        let held = List.exists (fun d -> d.d_held () <> []) nodes in
        match (time_limit, next_time nodes world) with
        | Some limit, Some next when (not held) && next > limit -> Time_limit
        | _ -> ( match step s nodes world with None -> Quiet | Some _ -> go (n - 1))
    in
    go steps

  (* What is left: each node's held Events and next timer, and the world's
     next time. *)
  let pending s nodes world =
    let b = Buffer.create 256 in
    Printf.bprintf b "at %d:\n" (Clock.Manual.get s.clock);
    List.iter
      (fun d ->
        let held = d.d_held () in
        Printf.bprintf b "  %s: %d held, next timer %s\n" d.d_name (List.length held)
          (match d.d_next_deadline () with None -> "none" | Some t -> string_of_int t);
        List.iter
          (fun e ->
            Printf.bprintf b "    %d %s to %s (%s lane, answers %s)\n" e.held_id
              e.held_kind e.held_path
              (match e.held_lane with Store_lane -> "Store" | Worker_lane -> "Worker")
              (match e.held_answers with None -> "none" | Some a -> string_of_int a))
          held)
      nodes;
    Printf.bprintf b "  world: next %s\n"
      (match world.next_time () with None -> "none" | Some t -> string_of_int t);
    Buffer.contents b
end

(* ------------------------------------------------------------------------ *)
(* Generated runs and shrinking *)

(* How [check] runs. CAUSET_SEED and CAUSET_RUNS override [seed] and
   [runs]. *)
type config = {
  runs : int;  (** Runs that must pass. Run [i] draws from seed [seed + i]. *)
  seed : int;
  max_rejects : int option;  (** Rejected runs allowed; [None] is ten times [runs]. *)
  max_steps : int;  (** Root inputs a run may take, counted over every Node. *)
  max_shrinks : int;  (** Runs shrinking may make. *)
  determinism : bool;
      (** Makes every passing run a second time from its own choices, and fails the first
          whose executions differ. *)
}

let default =
  {
    runs = 100;
    seed = 0;
    max_rejects = None;
    max_steps = 10_000;
    max_shrinks = 2_000;
    determinism = true;
  }

(* A failing run, found and shrunk. *)
type found = {
  seed : int;  (** The seed of the run that failed first. *)
  key : string;  (** What failed, the same for the original and the shrunk run. *)
  choices : int list;  (** The shrunk run's choices: [replay choices f] repeats it. *)
  original : int;  (** How many choices the original run made. *)
  shrinks : int;  (** How many runs shrinking made. *)
  found_max_steps : int;
  detail : string option;
      (** For a run that was not deterministic, where its two makings first differ. *)
}

type outcome =
  | Passed
  | Was_rejected
  | Failed_with of { key : string; choices : int list }

let with_limit max f =
  let r = Domain.DLS.get run_state in
  let previous = r.max_steps in
  r.max_steps <- Some max;
  Fun.protect ~finally:(fun () -> r.max_steps <- previous) f

let begin_run () = (Domain.DLS.get run_state).steps <- 0

let attempt f rng =
  begin_run ();
  match f rng with
  | () -> Passed
  | exception Rejected -> Was_rejected
  | exception Failed { key; _ } ->
      Failed_with { key; choices = Option.get (Rng.choices rng) }
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      let key = "panic at " ^ place (raise_location bt) (exn_message e) in
      Failed_with { key; choices = Option.get (Rng.choices rng) }

(* [attempt], keeping the run's executions when [traced]. *)
let attempt_traced f rng traced =
  let r = Domain.DLS.get run_state in
  if traced then r.trace <- Some [];
  let outcome = attempt f rng in
  let trace = Option.fold ~none:[] ~some:List.rev r.trace in
  r.trace <- None;
  (outcome, trace)

(* Where a run's second making first differs from its first, which passed. *)
let difference first second outcome =
  match outcome with
  | Failed_with { key; _ } ->
      Some (Printf.sprintf "the first passed and the second failed (%s)" key)
  (* The same choices led one run to pass and the other to reject: whatever
     the traces show, the two runs did not agree. *)
  | Was_rejected -> Some "the first passed and the second was rejected"
  | Passed ->
      let rec go i = function
        | a :: rest, b :: rest' ->
            if String.equal a b then go (i + 1) (rest, rest')
            else Some (Printf.sprintf "at step %d: first %s, then %s" i a b)
        | [], [] -> None
        | _ ->
            Some
              (Printf.sprintf
                 "the first made %d executions, refusals and inline calls, the second %d"
                 (List.length first) (List.length second))
      in
      go 1 (first, second)

(* Shorter, or as long and smaller at the first difference. *)
let smaller a b = compare (List.length a, a) (List.length b, b) < 0
let pair_window = 4
let pair_values = 4

(* Passes over the choices until none makes progress or the budget is
   spent. An edit is kept only if the run fails with the same key and makes
   a smaller list of choices. *)
let shrink f key best budget =
  let best = ref (Array.of_list best) in
  let runs = ref 0 in
  let try_edit candidate =
    if !runs >= budget then false
    else (
      incr runs;
      match attempt f (Rng.replaying (Array.to_list candidate)) with
      | Failed_with { key = k; choices }
        when k = key && smaller choices (Array.to_list !best) ->
          best := Array.of_list choices;
          true
      | _ -> false)
  in
  let remove a i len =
    Array.append (Array.sub a 0 i) (Array.sub a (i + len) (Array.length a - i - len))
  in
  let rec pass () =
    let before = Array.copy !best in
    (* 1. Delete a span: removes steps. *)
    for len = 8 downto 1 do
      let i = ref 0 in
      while !i + len <= Array.length !best do
        if not (try_edit (remove !best !i len)) then incr i
      done
    done;
    (* 2. Set a span to 0. *)
    List.iter
      (fun len ->
        let i = ref 0 in
        while !i + len <= Array.length !best do
          if Array.exists (fun c -> c <> 0) (Array.sub !best !i len) then (
            let c = Array.copy !best in
            Array.fill c !i len 0;
            ignore (try_edit c));
          incr i
        done)
      [ 8; 4; 2; 1 ];
    (* 3. Lower one choice: the smallest value that still fails. *)
    let i = ref 0 in
    while !i < Array.length !best do
      let low = ref 0 and high = ref !best.(!i) in
      while !low < !high && !i < Array.length !best do
        let mid = !low + ((!high - !low) / 2) in
        let c = Array.copy !best in
        c.(!i) <- mid;
        if try_edit c then high := mid else low := mid + 1
      done;
      incr i
    done;
    (* 4. Two choices at once, only when no single edit helps. *)
    let progressed = ref false in
    (if !best = before then
       try
         for i = 0 to Array.length !best - 1 do
           let current = !best.(i) in
           List.iter
             (fun lowered ->
               if lowered < current then
                 for j = i + 1 to min (Array.length !best) (i + 1 + pair_window) - 1 do
                   for value = 0 to pair_values do
                     if value <> !best.(j) then (
                       let c = Array.copy !best in
                       c.(i) <- lowered;
                       c.(j) <- value;
                       if try_edit c then (
                         progressed := true;
                         raise Exit))
                   done
                 done)
             [ 0; current / 2 ]
         done
       with Exit -> ());
    if (!best = before && not !progressed) || !runs >= budget then
      (Array.to_list !best, !runs)
    else pass ()
  in
  pass ()

(* What [check] does before reporting: the first failing run, shrunk, or
   [None] when [config.runs] runs passed. It applies no environment
   overrides. *)
let search (config : config) f =
  Printexc.record_backtrace true;
  with_limit config.max_steps (fun () ->
      let max_rejects = Option.value config.max_rejects ~default:(config.runs * 10) in
      let rec go seed passed rejected =
        if passed >= config.runs then None
        else
          let rng = Rng.recording seed in
          match attempt_traced f rng config.determinism with
          | Passed, first -> (
              (* A run that made no Store, such as a projection's, has
                 nothing to compare. *)
              let choices = Option.get (Rng.choices rng) in
              let differs =
                if config.determinism && first <> [] then
                  let again, second = attempt_traced f (Rng.replaying choices) true in
                  difference first second again
                else None
              in
              match differs with
              | None -> go (seed + 1) (passed + 1) rejected
              | Some detail ->
                  Some
                    {
                      seed;
                      key = "not deterministic";
                      original = List.length choices;
                      choices;
                      shrinks = 0;
                      found_max_steps = config.max_steps;
                      detail = Some detail;
                    })
          | Was_rejected, _ ->
              if rejected + 1 > max_rejects then
                failwith
                  (Printf.sprintf
                     "%d runs rejected and %d passed: the generator rejects almost \
                      everything, so it tests almost nothing"
                     (rejected + 1) passed);
              go (seed + 1) passed (rejected + 1)
          | Failed_with { key; choices }, _ ->
              let original = List.length choices in
              let choices, shrinks = shrink f key choices config.max_shrinks in
              Some
                {
                  seed;
                  key;
                  choices;
                  original;
                  shrinks;
                  found_max_steps = config.max_steps;
                  detail = None;
                }
      in
      go config.seed 0 0)

(* Runs [f] on exactly [choices] (each clamped to its draw's bound, then 0
   past the end), under [config]'s step limit. *)
let replay ?(config = default) choices f =
  Printexc.record_backtrace true;
  with_limit config.max_steps (fun () ->
      begin_run ();
      f (Rng.replaying choices))

(* Runs [f] on a found failure's shrunk choices under the step limit it was
   found with: the failure, kept as a regression test. *)
let replay_found found f =
  replay ~config:{ default with max_steps = found.found_max_steps } found.choices f

let from_env (config : config) : config =
  let read name = Option.map int_of_string (Sys.getenv_opt name) in
  let config =
    match read "CAUSET_SEED" with Some seed -> { config with seed } | None -> config
  in
  match read "CAUSET_RUNS" with Some runs -> { config with runs } | None -> config

(* Runs [f] on generated choices until a run fails or [config.runs] runs
   passed. A failure is shrunk; then the seed and the shrunk choices are
   printed, and the shrunk run is made once more, so its own exception fails
   the test. Fails, too, when the shrunk run passes on that last replay (the
   run is not repeatable: are all its Services simulated?). *)
let check ?(config : config = default) f =
  let config = from_env config in
  match search config f with
  | None -> ()
  | Some { detail = Some detail; seed; original; _ } ->
      failwith
        (Printf.sprintf
           "causet_test.check: the run with seed %d is not deterministic: made twice \
            from the same %d choices, %s. Something in it reads what its choices do not \
            decide (the system clock, a Hashtbl's order, a thread's timing). Repeat it \
            with CAUSET_SEED=%d"
           seed original detail seed)
  | Some found ->
      let choices = String.concat "; " (List.map string_of_int found.choices) in
      let call =
        if found.found_max_steps = default.max_steps then
          Printf.sprintf "Causet_test.replay [%s] ..." choices
        else
          Printf.sprintf
            "Causet_test.replay ~config:{ Causet_test.default with max_steps = %d } [%s] \
             ..."
            found.found_max_steps choices
      in
      Printf.eprintf
        "causet_test.check: the run with seed %d failed (%s); shrunk from %d choices to \
         %d in %d runs. Repeat it with CAUSET_SEED=%d or\n\
        \    %s\n\
         %!"
        found.seed found.key found.original (List.length found.choices) found.shrinks
        found.seed call;
      replay ~config:{ config with max_steps = found.found_max_steps } found.choices f;
      failwith
        (Printf.sprintf
           "the shrunk run of seed %d (%s) passed when replayed: the run is not \
            repeatable (are all its Services simulated?)"
           found.seed found.key)
