open Ids
open Observe
open Kernel_types
open Kernel_observe

(* ------------------------------------------------------------------------ *)
(* Reducer contexts *)

module Ctx = struct
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) ctx

  let live ctx what =
    if ctx.cphase = Finished then
      invalid_arg ("causet: " ^ what ^ " after the reducer returned")

  let mutating ctx what =
    match ctx.cphase with
    | Mutating -> ()
    | Emitting -> invalid_arg ("causet: " ^ what ^ " after the reducer started emitting")
    | Finished -> invalid_arg ("causet: " ^ what ^ " after the reducer returned")

  (* The component's State. *)
  let state ctx =
    live ctx "state";
    ctx.cstate

  (* Sets the component's State. Only before the first emission. In a Store
     it reaches the Store's State when the reducer returns, or raises: a raise
     after it keeps it, as a reducer's change in place would, since a fault
     rolls nothing back. Nothing reads the Store's State while a reducer
     runs. *)
  let set_state ctx s =
    mutating ctx "set_state";
    ctx.cstate <- s

  let update ctx f = set_state ctx (f (state ctx))
  let meta ctx = ctx.cmeta

  (* One counter per execution, shared with children called inline. *)
  let new_request ctx =
    mutating ctx "new_request";
    let session, execution, index =
      match ctx.backend with
      | In_store (_, exec) ->
          let i = exec.next_request in
          exec.next_request <- i + 1;
          (exec.x_session, exec.x_execution, i)
      | In_harness c ->
          let i = c.c_next_request in
          c.c_next_request <- i + 1;
          (c.c_session, c.c_execution, i)
    in
    { Request_id.session; execution; index }

  let rec slot_child name = function
    | [] -> None
    | (n, child) :: rest ->
        if String.equal n name then Some child else slot_child name rest

  let resolve_slot proto parent name uid =
    let def = proto.mounts.(parent) in
    match slot_child name def.children with
    | None -> failwith (Printf.sprintf "slot %s is not bound under %s" name def.path)
    | Some child ->
        if proto.mounts.(child).comp_uid <> uid then
          failwith (Printf.sprintf "slot %s holds another type" name);
        child

  let refused_inline core child (actions : 'a P.t) (action : 'a) at by now =
    let os = core.listen.on_action_refused in
    if Array.length os > 0 then
      let def = core.proto.mounts.(child) in
      let payload = P.payload actions action in
      tell os at Refused child
        (fun () ->
          {
            Refusal.path = def.path;
            instance = def.instance;
            message = P.name payload;
            kind = P.kind payload;
            sent = From_inline at;
            by;
            time = now;
          })
        (fun (o : Observe.t) r p -> o.action_refused r p)
        payload

  (* Runs the child's reducer inside this execution, and writes its new
     State back into this component's. It makes no execution of its own:
     observers are told of it within this one, or of its refusal. Under a
     Harness the call is recorded instead, and reported as [Reduced]. *)
  let reduce_child (type s svc a e o cs csvc ca ce co) (ctx : (s, svc, a, e, o) ctx)
      (slot : (s * svc * a * e * o, cs, csvc, ca, ce, co) child) (action : ca) =
    mutating ctx "reduce_child";
    let cc = slot.slot_comp in
    match ctx.backend with
    | In_harness c ->
        let payload = P.payload cc.actions action in
        Queue.push
          (Raw_inline
             { slot = slot.slot_name; call_kind = P.kind payload; call = payload })
          c.c_emissions;
        Reduced
    | In_store (core, _) -> (
        let proto = core.proto in
        let at = ctx.cmeta.id in
        let child = resolve_slot proto ctx.cmount slot.slot_name cc.uid in
        let def = proto.mounts.(child) in
        let now = ctx.cmeta.time in
        Bug.tag Reducer ctx.cmount;
        let parent_dyn = Dyn (ctx.ccomp.state_id, ctx.cstate) in
        let lens =
          match def.lens with Some l -> l | None -> invalid_arg "causet: no lens"
        in
        (* The checks, in order: the child's State is there, its root guard,
           the slot guard, the child's own check. The root guard runs only
           when the root is the caller, the one context that holds the whole
           State, read live from it; below it, the caller takes on what the
           guard checks ([Registry.admit]). *)
        match lens.lget parent_dyn with
        | None ->
            refused_inline core child cc.actions action at Absent now;
            Refused
        | Some child_dyn ->
            let root_admits =
              match def.root_guard with
              | Some guard when ctx.cmount = 0 ->
                  Bug.attribute Admission 0 (fun () ->
                      guard parent_dyn child_dyn (P.payload cc.actions action) now)
              | _ -> true
            in
            if not root_admits then (
              refused_inline core child cc.actions action at Root_guard now;
              Refused)
            else
              let guarded =
                match def.guard with
                | None -> true
                | Some guard ->
                    Bug.attribute Admission ctx.cmount (fun () ->
                        guard parent_dyn (P.payload cc.actions action) now)
              in
              if not guarded then (
                refused_inline core child cc.actions action at Guard now;
                Refused)
              else
                let cs = unwrap cc.state_id child_dyn in
                if
                  not
                    (Bug.attribute Admission child (fun () -> cc.is_enabled cs action now))
                then (
                  refused_inline core child cc.actions action at Component now;
                  Refused)
                else (
                  (let os = core.listen.on_inline_action in
                   if Array.length os > 0 then
                     tell os at Inline child
                       (fun () ->
                         {
                           Inline_call.path = def.path;
                           instance = def.instance;
                           message = cc.actions.name;
                           kind = cc.actions.kind action;
                           within = at;
                           time = now;
                           root_guard_skipped =
                             Option.is_some def.root_guard && ctx.cmount <> 0;
                         })
                       (fun (o : Observe.t) c p -> o.inline_action c p)
                       (P.payload cc.actions action));
                  let cctx =
                    {
                      cstate = cs;
                      cphase = Mutating;
                      cmeta = ctx.cmeta;
                      ccomp = cc;
                      backend = ctx.backend;
                      cmount = child;
                    }
                  in
                  (* The child's State goes back into this component's as the
                   child returns, or raises. *)
                  let back () =
                    cctx.cphase <- Finished;
                    if cctx.cstate != cs then
                      ctx.cstate <-
                        unwrap ctx.ccomp.state_id
                          (lens.lset parent_dyn (Dyn (cc.state_id, cctx.cstate)))
                  in
                  (match
                     Bug.attribute Reducer child (fun () ->
                         cc.reduce cctx action ctx.cmeta)
                   with
                  | () -> back ()
                  | exception e ->
                      let bt = Printexc.get_raw_backtrace () in
                      back ();
                      Printexc.raise_with_backtrace e bt);
                  Reduced))

  let emitting ctx what =
    live ctx what;
    ctx.cphase <- Emitting

  let push core exec ctx category target payload =
    let ordinal = claim_emission exec in
    let origin =
      {
        Meta.causal = exec.x_execution;
        component = core.proto.mounts.(ctx.cmount).instance;
        emission_ordinal = ordinal;
        lowering_ordinal = 0;
      }
    in
    Queue.push
      { q_category = category; q_target = target; q_payload = payload; q_origin = origin }
      exec.fifo

  (* Queues an Action for this component, checked for admission when its turn
     comes. *)
  let emit_action ctx action =
    emitting ctx "emit_action";
    let payload = P.payload ctx.ccomp.actions action in
    match ctx.backend with
    | In_store (core, exec) -> push core exec ctx Meta.Action ctx.cmount payload
    | In_harness c -> Queue.push (Raw_action payload) c.c_emissions

  (* Queues an Effect for this component's handler. *)
  let emit_effect ctx eff =
    emitting ctx "emit_effect";
    let payload = P.payload ctx.ccomp.effects eff in
    match ctx.backend with
    | In_store (core, exec) -> push core exec ctx Meta.Effect ctx.cmount payload
    | In_harness c -> Queue.push (Raw_effect payload) c.c_emissions

  (* Queues an Action for the child at [slot], run as its own execution when
     its turn comes. [reduce_child] calls it inline instead, and is the way
     for a parent to learn of a refusal. *)
  let to_child (type s svc a e o cs csvc ca ce co) (ctx : (s, svc, a, e, o) ctx)
      (slot : (s * svc * a * e * o, cs, csvc, ca, ce, co) child) (action : ca) =
    emitting ctx "to_child";
    let payload = P.payload slot.slot_comp.actions action in
    match ctx.backend with
    | In_store (core, exec) ->
        let child =
          resolve_slot core.proto ctx.cmount slot.slot_name slot.slot_comp.uid
        in
        push core exec ctx Meta.Action child payload
    | In_harness c ->
        Queue.push
          (Raw_to_child
             { slot = slot.slot_name; call_kind = P.kind payload; call = payload })
          c.c_emissions

  (* Lowered in place by the parent's lowerer for this mount. *)
  let emit_output ctx output =
    emitting ctx "emit_output";
    let payload = P.payload ctx.ccomp.outputs output in
    match ctx.backend with
    | In_harness c -> Queue.push (Raw_output payload) c.c_emissions
    | In_store (core, exec) ->
        let ordinal = claim_emission exec in
        let def = core.proto.mounts.(ctx.cmount) in
        let parent =
          match def.parent with
          | Some p -> p
          | None -> failwith "the root has no parent; its Output must be never"
        in
        let lower =
          match def.lower with
          | Some l -> l
          | None -> failwith (Printf.sprintf "no Output lowerer bound for %s" def.path)
        in
        let origin =
          {
            Meta.causal = exec.x_execution;
            component = def.instance;
            emission_ordinal = ordinal;
            lowering_ordinal = 0;
          }
        in
        Bug.tag Reducer ctx.cmount;
        Bug.attribute Reducer parent (fun () -> lower exec parent origin payload)
end

module Port = struct
  type 'a t = 'a port

  (* Queues an Action for the port's component. *)
  let emit_action port action =
    let origin =
      match port.pmode with
      | Lowering l ->
          let o = { l.origin with lowering_ordinal = l.next } in
          l.next <- l.next + 1;
          o
      | Adapting component ->
          {
            Meta.causal = port.pexec.x_execution;
            component;
            emission_ordinal = claim_emission port.pexec;
            lowering_ordinal = 0;
          }
    in
    Queue.push
      {
        q_category = Meta.Action;
        q_target = port.ptarget;
        q_payload = P.payload port.paction action;
        q_origin = origin;
      }
      port.pexec.fifo
end
