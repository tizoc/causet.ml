open Observe
open Kernel_types
open Kernel_observe
open Kernel_state
open Kernel_schedule
open Kernel_bus

(* ------------------------------------------------------------------------ *)
(* Effect contexts *)

(* Records the fault of [exn], raised with backtrace [bt], unless one is
   recorded already: the first wins. Its message is the exception's and its
   place where it was raised (an observer's exception inside a reducer
   carries its own); the bug reports made before it are told first, the bus
   closes, and the observers are told, their raises ignored: nothing they do
   can undo or unwind past it. *)
let record_fault core exn bt =
  if Option.is_none core.fault then (
    let exn, place =
      match exn with
      | Observer_panic { at; boundary; mount; exn; location } ->
          core.fault_location <- location;
          (exn, Some (Some at, Observer boundary, mount))
      | e ->
          core.fault_location <- raise_location bt;
          (e, None)
    in
    let message = exn_message exn in
    let at, phase, mount =
      match place with
      | Some p -> p
      | None ->
          let at, phase = fault_place core in
          (at, phase, fault_mount core)
    in
    (try flush_bugs core phase at mount with _ -> ());
    core.fault <- Some message;
    (* A faulted Store drains nothing again: its bus closes at once, as
       [close] does, so Services' sends fail with [Closed] rather than fill
       the worker lane. *)
    close_bus core.bus;
    let info = { Fault_info.phase; at; message } in
    notify_fault core info)

module Effect_ctx = struct
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) effect_ctx

  let live ctx =
    if not ctx.evalid then
      invalid_arg "causet: an Effect context used after its handler returned"

  let meta ctx = ctx.emeta

  (* The component's State, read-only; [None] if it was mounted through a
     fallible lens and is absent now. *)
  let state ctx =
    live ctx;
    Option.map (unwrap ctx.ecomp.state_id)
      (state_get ctx.ecore.proto ctx.ecore.state ctx.emount)

  (* The component's Service. *)
  let service ctx =
    live ctx;
    match service_get ctx.ecore.proto ctx.ecore.services ctx.emount with
    | Some s -> unwrap ctx.ecomp.service_id s
    | None ->
        failwith
          ("no Service projection reaches " ^ ctx.ecore.proto.mounts.(ctx.emount).path)

  (* Dispatches an Action to this component synchronously: admission first;
     an ordinal is claimed only for an admitted Action. *)
  let run_dispatch ctx action =
    let core = ctx.ecore in
    flush_bugs core Handler (Some ctx.emeta.id) ctx.emount;
    let payload = P.payload ctx.ecomp.actions action in
    match
      admit core ctx.emount payload (From_effect ctx.emeta.id) (Some ctx.emeta.id)
        core.last
    with
    | None -> Disabled
    | Some st ->
        let ordinal = ctx.e_next_emission in
        ctx.e_next_emission <- ordinal + 1;
        let origin =
          {
            Meta.causal = ctx.emeta.id;
            component = core.proto.mounts.(ctx.emount).instance;
            emission_ordinal = ordinal;
            lowering_ordinal = 0;
          }
        in
        ignore (run_action core ctx.emount st payload (Some origin) None);
        Executed

  (* An exception that leaves the work it ran faults the Store here, at its
     place, whether or not the handler catches it; the execution stack and
     depth are restored to this call's before it goes on. Once faulted, it
     runs nothing. *)
  let dispatch ctx action =
    live ctx;
    let core = ctx.ecore in
    if Option.is_some core.fault then (Faulted : dispatch_outcome)
    else
      let open_ = core.open_ and depth = core.depth in
      match run_dispatch ctx action with
      | outcome -> outcome
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          record_fault core e bt;
          core.open_ <- open_;
          core.depth <- depth;
          core.special <- false;
          Printexc.raise_with_backtrace e bt

  (* A reply handle for the work this Effect starts. *)
  let reply ctx (msg : 'ev P.t) : 'ev reply =
    live ctx;
    let core = ctx.ecore in
    let def = core.proto.mounts.(ctx.emount) in
    if not (registered core.proto ctx.emount msg) then
      failwith (Printf.sprintf "%s never registered Event %s" def.path msg.name);
    let sink = { ssession = core.session; sproto = core.proto; sbus = core.bus } in
    { rsink = sink; rmessage = msg; rinstance = def.instance; reffect = ctx.emeta.id }
end
