open Ids
open Observe
open Kernel_types
open Kernel_observe
open Kernel_state

(* ------------------------------------------------------------------------ *)
(* Scheduling *)

(* An execution's time: the clock's reading, never before the execution
   before it, so times never go back whatever the clock does; executions
   in the same instant share it. *)
let tick core now = max core.last now

let clock_time core =
  enter_special core (-1) Clock 0;
  let now = Clock.now core.clock in
  let time = tick core now in
  core.special <- false;
  flush_bugs core Clock None 0;
  time

let next_meta core origin time : Meta.t =
  let time = match time with Some t -> t | None -> clock_time core in
  core.last <- time;
  let id = core.next_id in
  (* Exhaustion is a fault, never a wrap-around. *)
  if id = max_int then failwith "execution IDs exhausted";
  core.next_id <- id + 1;
  { id; depth = core.depth + 1; time; origin }

let push_open core entry = core.open_ <- entry :: core.open_
let pop_open core = core.open_ <- List.tl core.open_

(* Admission at [now]. The checks, in order: the mount's State is there, its
   root guard, its parent's slot guard, its own check. The parent's State,
   and the mount's through the parent's slot lens, come from one walk. A
   root guard's bug reports are placed at the root, a slot guard's at the
   parent. *)
let admitted core target payload now =
  let proto = core.proto in
  let def = proto.mounts.(target) in
  let root = core.state in
  match def.parent with
  | None -> if def.admit root payload now then Ok root else Error Component
  | Some parent -> (
      match state_get proto root parent with
      | None -> Error Absent
      | Some ps -> (
          match (lens_of proto target).lget ps with
          | None -> Error Absent
          | Some own ->
              let root_admits =
                match def.root_guard with
                | None -> true
                | Some guard ->
                    Bug.attribute Admission 0 (fun () -> guard root own payload now)
              in
              if not root_admits then Error Root_guard
              else
                let slot_admits =
                  match def.guard with
                  | None -> true
                  | Some guard ->
                      Bug.attribute Admission parent (fun () -> guard ps payload now)
                in
                if not slot_admits then Error Guard
                else if def.admit own payload now then Ok own
                else Error Component))

let refused core target payload sent by now =
  let os = core.listen.on_action_refused in
  if Array.length os > 0 then (
    let def = core.proto.mounts.(target) in
    let at = match sent_execution sent with Some at -> at | None -> -1 in
    enter_special core at (Observer Refused) target;
    (* The kind is read only to tell an observer, in its phase. *)
    let refusal =
      {
        Refusal.path = def.path;
        instance = def.instance;
        message = P.name payload;
        kind = P.kind payload;
        sent;
        by;
        time = now;
      }
    in
    for i = 0 to Array.length os - 1 do
      (Array.unsafe_get os i).action_refused refusal payload
    done;
    core.special <- false;
    flush_bugs core (Observer Refused) (sent_execution sent) target)

(* The target's State when the Action is admitted, read once for its check
   and its reducer: nothing changes the Store's State in between. *)
let admit core target payload sent at now =
  push_open core (at, Admission, target);
  let result = admitted core target payload now in
  pop_open core;
  flush_bugs core Admission (sent_execution sent) target;
  match result with
  | Ok own -> Some own
  | Error by ->
      refused core target payload sent by now;
      None

let execution category def payload meta answers =
  {
    Execution.category;
    path = def.path;
    instance = def.instance;
    message = P.name payload;
    kind = P.kind payload;
    meta;
    answers;
  }

(* An execution's record, made only once an observer is to be told of it:
   [no_execution] until then. *)
let no_execution =
  {
    Execution.category = Meta.Action;
    path = "";
    instance = 0L;
    message = "";
    kind = "";
    meta = { Meta.id = 0; depth = 0; time = 0; origin = None };
    answers = None;
  }

let rec run_action core target st payload origin time =
  let proto = core.proto in
  let def = proto.mounts.(target) in
  let meta = next_meta core origin time in
  core.executions <- core.executions + 1;
  let observed = ref no_execution in
  let os = core.listen.on_action_start in
  if Array.length os > 0 then (
    observed := execution Meta.Action def payload meta None;
    observe2 core os meta.id target Start
      (fun (o : Observe.t) e p -> o.action_start e p)
      !observed payload);
  let exec = new_exec core.session meta.id in
  push_open core (Some meta.id, Reducer, target);
  def.run core target exec st payload meta;
  pop_open core;
  flush_bugs core Reducer (Some meta.id) target;
  let os = core.listen.on_action_finish in
  if Array.length os > 0 then (
    if !observed == no_execution then
      observed := execution Meta.Action def payload meta None;
    observe core os meta.id target Finish
      (fun (o : Observe.t) e -> o.action_finish e)
      !observed);
  drain_fifo core exec;
  let os = core.listen.on_action_settled in
  if Array.length os > 0 then (
    if !observed == no_execution then
      observed := execution Meta.Action def payload meta None;
    observe core os meta.id target Settled
      (fun (o : Observe.t) e -> o.action_settled e)
      !observed);
  meta.id

and execute_action core target payload origin =
  let now = core.last in
  match admit core target payload (Queued origin) None now with
  | Some st -> ignore (run_action core target st payload (Some origin) None)
  | None -> ()

and run_effect core target payload origin =
  let def = core.proto.mounts.(target) in
  let meta = next_meta core (Some origin) None in
  core.executions <- core.executions + 1;
  let observed = ref no_execution in
  let os = core.listen.on_effect_start in
  if Array.length os > 0 then (
    observed := execution Meta.Effect def payload meta None;
    observe2 core os meta.id target Start
      (fun (o : Observe.t) e p -> o.effect_start e p)
      !observed payload);
  core.depth <- core.depth + 1;
  push_open core (Some meta.id, Handler, target);
  def.handle core target payload meta;
  (* The handler caught a dispatch's exception: nothing that was interrupted
     resumes, and nothing after it runs. *)
  if Option.is_some core.fault then raise Fault_unwind;
  pop_open core;
  flush_bugs core Handler (Some meta.id) target;
  core.depth <- core.depth - 1;
  let os = core.listen.on_effect_finish in
  if Array.length os > 0 then (
    if !observed == no_execution then
      observed := execution Meta.Effect def payload meta None;
    observe core os meta.id target Finish
      (fun (o : Observe.t) e -> o.effect_finish e)
      !observed)

and drain_fifo core exec =
  core.depth <- core.depth + 1;
  Queue.iter
    (fun q ->
      match q.q_category with
      | Meta.Action -> execute_action core q.q_target q.q_payload q.q_origin
      | Meta.Effect -> run_effect core q.q_target q.q_payload q.q_origin
      | Meta.Event -> invalid_arg "causet: Events are root inputs, never queued")
    exec.fifo;
  core.depth <- core.depth - 1

(* Runs one Event as a root input; returns its execution, its mount and its
   kind. *)
let run_event core env =
  let proto = core.proto in
  if env.e_session <> core.session then failwith "Event from another session";
  let target =
    match Hashtbl.find_opt proto.by_instance env.e_instance with
    | Some t -> t
    | None ->
        failwith (Format.asprintf "no mount with instance %a" pp_instance env.e_instance)
  in
  let def = proto.mounts.(target) in
  let binding =
    match List.find_opt (fun b -> b.ev_uid = env.e_event) def.events with
    | Some b -> b
    | None -> failwith (Printf.sprintf "%s never registered Event %s" def.path env.e_kind)
  in
  let meta = next_meta core None None in
  core.executions <- core.executions + 1;
  let kind = P.kind env.e_payload in
  let observed = ref no_execution in
  let os = core.listen.on_event_start in
  if Array.length os > 0 then (
    observed := execution Meta.Event def env.e_payload meta env.e_effect;
    observe2 core os meta.id target Start
      (fun (o : Observe.t) e p -> o.event_start e p)
      !observed env.e_payload);
  let exec = new_exec core.session meta.id in
  push_open core (Some meta.id, Adapter, target);
  binding.adapt exec target def.instance env.e_payload;
  pop_open core;
  flush_bugs core Adapter (Some meta.id) target;
  let os = core.listen.on_event_finish in
  if Array.length os > 0 then (
    if !observed == no_execution then
      observed := execution Meta.Event def env.e_payload meta env.e_effect;
    observe core os meta.id target Finish
      (fun (o : Observe.t) e -> o.event_finish e)
      !observed);
  drain_fifo core exec;
  let os = core.listen.on_event_settled in
  if Array.length os > 0 then (
    if !observed == no_execution then
      observed := execution Meta.Event def env.e_payload meta env.e_effect;
    observe core os meta.id target Settled
      (fun (o : Observe.t) e -> o.event_settled e)
      !observed);
  (meta.id, meta.time, target, kind)
