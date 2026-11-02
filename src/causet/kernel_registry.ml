open Kernel_bus
open Ids
open Kernel_types
open Kernel_state

(* ------------------------------------------------------------------------ *)
(* Registration *)

let new_mount (type s svc a e o) (comp : (s, svc, a, e, o) component) path parent ~lens
    ~service ~guard ~lower =
  {
    path;
    instance = Xxh3.hash64 path;
    parent;
    chain = [||];
    comp_uid = comp.uid;
    has_effects = not (P.is_never comp.effects);
    children = [];
    lens;
    service;
    guard;
    root_guard = None;
    lower;
    events = [];
    run =
      (fun core mount exec st payload meta ->
        let s = unwrap comp.state_id st in
        let a = P.get_exn comp.actions payload in
        let ctx =
          {
            cstate = s;
            cphase = Mutating;
            cmeta = meta;
            ccomp = comp;
            backend = In_store (core, exec);
            cmount = mount;
          }
        in
        (* The reducer's State reaches the Store's as it returns, or raises. *)
        let finish () =
          ctx.cphase <- Finished;
          if ctx.cstate != s then
            core.state <-
              state_set core.proto core.state mount (Dyn (comp.state_id, ctx.cstate))
        in
        match comp.reduce ctx a meta with
        | () -> finish ()
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            finish ();
            Printexc.raise_with_backtrace e bt);
    handle =
      (fun core mount payload meta ->
        let e = P.get_exn comp.effects payload in
        let ectx =
          {
            ecore = core;
            emount = mount;
            emeta = meta;
            e_next_emission = 0;
            ecomp = comp;
            evalid = true;
          }
        in
        Fun.protect
          ~finally:(fun () -> ectx.evalid <- false)
          (fun () -> comp.handle ectx e meta));
    admit =
      (fun st payload now ->
        comp.is_enabled (unwrap comp.state_id st) (P.get_exn comp.actions payload) now);
  }

let error b p = b.errors <- b.errors @ [ p ]

module Registrar = struct
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) registrar

  let check_live r =
    if not r.rlive then
      invalid_arg
        (Printf.sprintf "causet: %s's registrar used after its register returned"
           (Dynarray.get r.builder.bmounts r.rmount).path)

  (* Runs [comp]'s [register] for the mount at [index], with a registrar
     live only while it runs. *)
  let run_register b index comp =
    let r = { builder = b; rmount = index; rcomp = comp; rlive = true } in
    Fun.protect ~finally:(fun () -> r.rlive <- false) (fun () -> comp.register r)

  (* Declares the child at [slot], its State projected by [lens], its Service
     by [service]; a [guard] on every route into it, reading this
     component's State; and the lowerer for its Outputs, required when it has
     any. *)
  let child (type s svc a e o cs csvc ca ce co) (r : (s, svc, a, e, o) registrar)
      (slot : (s * svc * a * e * o, cs, csvc, ca, ce, co) child) ~(lens : (s, cs) lens)
      ?(service : (svc -> csvc) option) ?(guard : (s -> ca -> timestamp -> bool) option)
      ?(on_output :
         ((s * svc * a * e * o, cs, csvc, ca, ce, co) child -> co -> a port -> unit)
         option) () =
    check_live r;
    let b = r.builder in
    let parent_def = Dynarray.get b.bmounts r.rmount in
    let path =
      if parent_def.path = "/" then "/" ^ slot.slot_name
      else parent_def.path ^ "/" ^ slot.slot_name
    in
    if List.mem_assoc slot.slot_name parent_def.children then
      error b (Slot_bound_twice path)
    else
      let cc = slot.slot_comp and pc = r.rcomp in
      if on_output = None && not (P.is_never cc.outputs) then error b (No_lowerer path);
      let elens =
        {
          lget =
            (fun d ->
              Option.map (fun c -> Dyn (cc.state_id, c)) (lens.get (unwrap pc.state_id d)));
          lset =
            (fun d c ->
              Dyn (pc.state_id, lens.set (unwrap pc.state_id d) (unwrap cc.state_id c)));
        }
      in
      let eservice =
        Option.map (fun f d -> Dyn (cc.service_id, f (unwrap pc.service_id d))) service
      in
      let eguard =
        Option.map
          (fun g d p now -> g (unwrap pc.state_id d) (P.get_exn cc.actions p) now)
          guard
      in
      let elower =
        Option.map
          (fun f exec parent origin p ->
            let o = P.get_exn cc.outputs p in
            f slot o
              {
                pexec = exec;
                ptarget = parent;
                pmode = Lowering { origin; next = 0 };
                paction = pc.actions;
              })
          on_output
      in
      let index = Dynarray.length b.bmounts in
      Dynarray.add_last b.bmounts
        (new_mount cc path (Some r.rmount) ~lens:(Some elens) ~service:eservice
           ~guard:eguard ~lower:elower);
      parent_def.children <- parent_def.children @ [ (slot.slot_name, index) ];
      run_register b index cc

  (* Accepts Events of [msg]'s type, turned into this component's Actions by
     [adapt]. *)
  let event (type s svc a e o ev) (r : (s, svc, a, e, o) registrar) (msg : ev P.t)
      (adapt : ev -> a port -> unit) =
    check_live r;
    let def = Dynarray.get r.builder.bmounts r.rmount in
    let uid = Type.Id.uid msg.id in
    if List.exists (fun b -> b.ev_uid = uid) def.events then
      error r.builder (Event_registered_twice { path = def.path; event = msg.name })
    else
      def.events <-
        def.events
        @ [
            {
              ev_uid = uid;
              ev_name = msg.name;
              adapt =
                (fun exec mount instance p ->
                  adapt (P.get_exn msg p)
                    {
                      pexec = exec;
                      ptarget = mount;
                      pmode = Adapting instance;
                      paction = r.rcomp.actions;
                    });
            };
          ]
end

let next_protocol = Atomic.make 1

(* A frozen application definition, typed by its root State and Services. *)
type ('s, 'svc) protocol_t = {
  p : protocol;
  root_state_id : 's Type.Id.t;
  root_service_id : 'svc Type.Id.t;
}

(* A [Registry.admit] call, resolved by [freeze]. *)
type 's pending_root_guard = {
  g_root : int;  (** The uid of the component the path starts at. *)
  g_names : string list;  (** innermost first *)
  g_comp : int;  (** The uid of the component the path's type says. *)
  g_admit : 's -> dyn -> P.payload -> timestamp -> bool;
}

type ('s, 'svc) registry = {
  rbuilder : builder;
  mutable root_ids : ('s Type.Id.t * 'svc Type.Id.t) option;
  mutable root_guards : 's pending_root_guard list;  (** newest first *)
}

(* Resolves a root guard's path among [mounts] and places its guard in
   [guards], by mount, or says what is wrong with it. *)
let place_root_guard mounts guards root_state_id g =
  let names = List.rev g.g_names in
  let path = "/" ^ String.concat "/" names in
  if g.g_root <> mounts.(0).comp_uid then Error (Root_guard_other_component "/")
  else
    let rec walk index = function
      | [] -> Some index
      | name :: rest -> (
          match List.assoc_opt name mounts.(index).children with
          | None -> None
          | Some child -> walk child rest)
    in
    match walk 0 names with
    | None -> Error (Root_guard_no_mount path)
    | Some index ->
        if mounts.(index).comp_uid <> g.g_comp then
          Error (Root_guard_other_component path)
        else if List.is_empty names then Error Root_guard_on_root
        else if Option.is_some guards.(index) then Error (Root_guard_twice path)
        else (
          guards.(index) <-
            Some (fun root own p now -> g.g_admit (unwrap root_state_id root) own p now);
          Ok ())

module Registry = struct
  type ('s, 'svc) t = ('s, 'svc) registry

  let create () =
    {
      rbuilder = { bmounts = Dynarray.create (); errors = [] };
      root_ids = None;
      root_guards = [];
    }

  (* Mounts the root component at [/], which registers the whole tree. *)
  let root (type s svc a e o) (reg : (s, svc) registry)
      (comp : (s, svc, a, e, o) component) =
    let b = reg.rbuilder in
    if Dynarray.length b.bmounts > 0 then error b Root_registered_twice
    else (
      if not (P.is_never comp.outputs) then error b Root_has_outputs;
      reg.root_ids <- Some (comp.state_id, comp.service_id);
      Dynarray.add_last b.bmounts
        (new_mount comp "/" None ~lens:None ~service:None ~guard:None ~lower:None);
      Registrar.run_register b 0 comp)

  (* Sets a root guard on the mount at [path], resolved by [freeze]: a check
     of the whole State, the mount's own State, its Action and the time. *)
  let admit (type s svc ds dsvc a e o) (reg : (s, svc) registry)
      (path : (ds, dsvc, a, e, o) path) (guard : s -> ds -> a -> timestamp -> bool) =
    let comp = path.pcomp in
    let g =
      {
        g_root = path.proot;
        g_names = path.names;
        g_comp = comp.uid;
        g_admit =
          (fun root own p now ->
            guard root (unwrap comp.state_id own) (P.get_exn comp.actions p) now);
      }
    in
    reg.root_guards <- g :: reg.root_guards

  (* Validates the registration into a Protocol, or reports everything wrong
     with it. *)
  let freeze reg =
    let b = reg.rbuilder in
    let mounts = Dynarray.to_array b.bmounts in
    let errors = ref b.errors in
    let add p = errors := !errors @ [ p ] in
    if Array.length mounts = 0 then add No_root;
    Array.iteri
      (fun i a ->
        for j = i + 1 to Array.length mounts - 1 do
          if Int64.equal a.instance mounts.(j).instance then
            add (Instance_collision { first = a.path; second = mounts.(j).path })
        done)
      mounts;
    (* Each root guard in the order it was set, placed on a copy of its
       mount once the registration is valid: a Protocol frozen earlier keeps
       its own. *)
    let guards = Array.make (Array.length mounts) None in
    (match reg.root_ids with
    | Some (root_state_id, _) when Array.length mounts > 0 ->
        List.iter
          (fun g ->
            match place_root_guard mounts guards root_state_id g with
            | Ok () -> ()
            | Error p -> add p)
          (List.rev reg.root_guards)
    | _ -> ());
    Array.iter
      (fun def ->
        if def.has_effects then
          let rec walk current =
            match mounts.(current).parent with
            | None -> ()
            | Some parent ->
                if mounts.(current).service = None then
                  add
                    (No_service_projection { path = def.path; at = mounts.(current).path })
                else walk parent
          in
          walk
            ( Array.length (Array.of_list []) |> fun _ ->
              let rec find k = if mounts.(k) == def then k else find (k + 1) in
              find 0 ))
      mounts;
    match (!errors, reg.root_ids) with
    | [], Some (root_state_id, root_service_id) ->
        let mounts =
          Array.mapi
            (fun index def ->
              match guards.(index) with
              | None -> def
              | Some _ as root_guard -> { def with root_guard })
            mounts
        in
        Array.iteri
          (fun index def ->
            let rec up current acc =
              match mounts.(current).parent with
              | None -> acc
              | Some p -> up p (current :: acc)
            in
            def.chain <- Array.of_list (up index []))
          mounts;
        let by_instance = Hashtbl.create (Array.length mounts) in
        Array.iteri (fun i m -> Hashtbl.replace by_instance m.instance i) mounts;
        Ok
          {
            p = { pid = Atomic.fetch_and_add next_protocol 1; mounts; by_instance };
            root_state_id;
            root_service_id;
          }
    | errors, _ -> Error (if List.is_empty errors then [ No_root ] else errors)
end

type resolve_error =
  | No_mount of { name : string; under : string }
  | Other_component of string

module Path = struct
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) path

  (* The path of the root mount, [/]. *)
  let root comp = { names = []; pcomp = comp; proot = comp.uid }

  (* The path of the child at [slot]. *)
  let child path slot =
    { names = slot.slot_name :: path.names; pcomp = slot.slot_comp; proot = path.proot }
end

module Protocol = struct
  type ('s, 'svc) t = ('s, 'svc) protocol_t

  (* The mount at [path], as a handle for Stores of this Protocol. A path
     that starts at another root is refused at [/], as [Registry.admit]'s is
     at freeze. *)
  let resolve (proto : _ protocol_t) (path : _ path) =
    let p = proto.p in
    let rec walk index = function
      | [] -> Ok index
      | name :: rest -> (
          match List.assoc_opt name p.mounts.(index).children with
          | None -> Error (No_mount { name; under = p.mounts.(index).path })
          | Some child -> walk child rest)
    in
    if path.proot <> p.mounts.(0).comp_uid then Error (Other_component "/")
    else
      match walk 0 (List.rev path.names) with
      | Error e -> Error e
      | Ok index ->
          if p.mounts.(index).comp_uid <> path.pcomp.uid then
            Error (Other_component p.mounts.(index).path)
          else
            Ok
              {
                index;
                mprotocol = p.pid;
                minstance = p.mounts.(index).instance;
                mcomp = path.pcomp;
              }

  let resolve_exn proto path =
    match resolve proto path with
    | Ok m -> m
    | Error (No_mount { name; under }) ->
        invalid_arg (Printf.sprintf "no mount %s under %s" name under)
    | Error (Other_component path) -> invalid_arg (path ^ " holds another component type")

  let path_of (proto : _ protocol_t) mount = proto.p.mounts.(mount.index).path
  let instance_of mount = mount.minstance
end

module Event_bus = struct
  (* The worker lane's capacity on the bus [Store.start] and
     [Store.open_protocol] make. *)
  let default_worker_capacity = 1024

  (* A bus for one Store of [proto], with a fresh session. *)
  let create (proto : _ protocol_t) ~worker_capacity =
    let session = (1 lsl 61) lor Atomic.fetch_and_add next_session 1 in
    build_bus proto.p worker_capacity session None

  (* A bus with the session given: two runs that give their Stores the same
     sessions make the same request IDs. *)
  let with_session (proto : _ protocol_t) ~worker_capacity session =
    build_bus proto.p worker_capacity session None

  (* A held bus, for tests and simulations: every accepted send waits until
     the returned handle releases it into its lane or discards it. *)
  let held (proto : _ protocol_t) ~worker_capacity session =
    let sink, inbox =
      build_bus proto.p worker_capacity session (Some { next_held = 0; waiting = [] })
    in
    (sink, inbox, { Held.hbus = sink.sbus; hproto = proto.p })
end
