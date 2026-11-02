open Observe
open Kernel_types

(* ------------------------------------------------------------------------ *)
(* Observers, bugs and faults *)

let component_name c = c.name
let child name comp = { slot_name = name; slot_comp = comp }
let child_name slot = slot.slot_name

(* Which callbacks each observer has: those it did not leave as
   [Observe.none]'s, read as it is installed. *)
let listeners_of all =
  let n = Observe.none in
  let having own = Array.of_list (List.filter own all) in
  {
    all;
    on_action_start = having (fun o -> o.action_start != n.action_start);
    on_action_finish = having (fun o -> o.action_finish != n.action_finish);
    on_action_settled = having (fun o -> o.action_settled != n.action_settled);
    on_effect_start = having (fun o -> o.effect_start != n.effect_start);
    on_effect_finish = having (fun o -> o.effect_finish != n.effect_finish);
    on_event_start = having (fun o -> o.event_start != n.event_start);
    on_event_finish = having (fun o -> o.event_finish != n.event_finish);
    on_event_settled = having (fun o -> o.event_settled != n.event_settled);
    on_action_refused = having (fun o -> o.action_refused != n.action_refused);
    on_inline_action = having (fun o -> o.inline_action != n.inline_action);
    on_fault = having (fun o -> o.fault != n.fault);
    on_bus = having (fun o -> o.bus != n.bus);
    on_bug = having (fun o -> o.bug != n.bug);
  }

(* The phase of an observer callback at each boundary, made once. *)
let observer_phase = function
  | Start -> Observer Start
  | Finish -> Observer Finish
  | Settled -> Observer Settled
  | Bus -> Observer Bus
  | Refused -> Observer Refused
  | Inline -> Observer Inline

(* An exception in a bug or fault callback is ignored. *)
let notify_bug core report =
  let os = core.listen.on_bug in
  for i = 0 to Array.length os - 1 do
    try (Array.unsafe_get os i).bug report with _ -> ()
  done

let notify_fault core info =
  let os = core.listen.on_fault in
  for i = 0 to Array.length os - 1 do
    try (Array.unsafe_get os i).fault info with _ -> ()
  done

let flush_bugs core phase at mount =
  if Bug.pending () then
    List.iter
      (fun (p : Bug.pending) ->
        let phase, mount = Option.value p.tag ~default:(phase, mount) in
        let sequence = core.bugs in
        core.bugs <- sequence + 1;
        let def = core.proto.mounts.(mount) in
        let place =
          { Bug_place.phase; at; component = def.instance; path = def.path; sequence }
        in
        notify_bug core (Bug.into_report p (Some place)))
      (Bug.take ())

let enter_special core at phase mount =
  core.special <- true;
  core.special_at <- at;
  core.special_phase <- phase;
  core.special_mount <- mount

(* The observers [os] at [boundary] of execution [at] ([-1] for none) on
   [mount], each called with [call], then the reports they made placed.
   [call] is a closed function, so passing it allocates nothing. *)
let observe core os at mount boundary call x =
  enter_special core at (observer_phase boundary) mount;
  for i = 0 to Array.length os - 1 do
    call (Array.unsafe_get os i) x
  done;
  core.special <- false;
  if Bug.pending () then
    flush_bugs core (observer_phase boundary) (if at < 0 then None else Some at) mount

let observe2 core os at mount boundary call x y =
  enter_special core at (observer_phase boundary) mount;
  for i = 0 to Array.length os - 1 do
    call (Array.unsafe_get os i) x y
  done;
  core.special <- false;
  if Bug.pending () then
    flush_bugs core (observer_phase boundary) (if at < 0 then None else Some at) mount

(* Calls the observers [os] at an inline call or an inline refusal, inside
   the parent's reducer, which runs within execution [at]: no special phase
   is set, since a reducer may catch what they raise; an exception carries
   its place instead, and reports their tag. [make] builds the record inside,
   since reading the Action's kind is part of telling them. *)
let tell os at boundary mount make call (payload : P.payload) =
  match
    let record = make () in
    for i = 0 to Array.length os - 1 do
      call (Array.unsafe_get os i) record payload
    done
  with
  | () -> Bug.tag (observer_phase boundary) mount
  | exception (Observer_panic _ as e) ->
      Bug.tag (observer_phase boundary) mount;
      raise e
  | exception exn ->
      let location = raise_location (Printexc.get_raw_backtrace ()) in
      Bug.tag (observer_phase boundary) mount;
      raise (Observer_panic { at; boundary; mount; exn; location })

let fault_place core =
  if core.special then
    ((if core.special_at < 0 then None else Some core.special_at), core.special_phase)
  else match core.open_ with (at, phase, _) :: _ -> (at, phase) | [] -> (None, Admission)

let fault_mount core =
  if core.special then core.special_mount
  else match core.open_ with (_, _, m) :: _ -> m | [] -> 0
