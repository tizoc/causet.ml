open Kernel_registry
open Kernel_readiness
open Ids
open Observe
open Kernel_types
open Kernel_observe
open Kernel_schedule
open Kernel_bus
open Kernel_effect

(* ------------------------------------------------------------------------ *)
(* The Store *)

type invariant_step =
  | Added  (** It was just registered, on the State as it is. *)
  | Restored
      (** It was moved into a new Store, with its memory, after a restart: the State is
          the restored one, and no root input has run yet. *)
  | Settled_step of {
      execution : execution_id;
      category : Meta.category;
      path : string;
      kind : string;
      time : timestamp;
    }  (** A root input ran with everything it caused. *)

type 's invariant = {
  iname : string;
  ifile : string;
  iline : int;
  check : 's -> invariant_step -> (unit, string) result;
  mutable disabled : bool;
}

(* A Store's invariants with their memory, taken out to move into the Store
   that replaces it after a restart. *)
type 's invariants = 's invariant list

module Invariants = struct
  type 's t = 's invariants

  let length = List.length
  let is_empty = List.is_empty
end

(* Runs an application: owns its State, Services and inbox, and executes
   root inputs one at a time, each to completion, on the calling thread. *)
type ('s, 'svc) store = {
  core : core;
  mutable invariants : 's invariant list;
  sproto_t : ('s, 'svc) protocol_t;
  mutable owner : (int * int) option;  (** The thread the bus was last told runs it. *)
}

module Store = struct
  type ('s, 'svc) t = ('s, 'svc) store

  (* A Store for [proto], starting from [state] with [services], draining
     [inbox]. *)
  let create ?clock (proto : ('s, 'svc) protocol_t) ~(state : 's) ~(services : 'svc) inbox
      =
    if not (Atomic.compare_and_set inbox.iclaimed false true) then
      invalid_arg "causet: this inbox already belongs to a Store"
    else if inbox.isink.sproto.pid <> proto.p.pid then (
      close_bus inbox.isink.sbus;
      Error Foreign_bus)
    else
      let clock = match clock with Some c -> c | None -> Clock.system () in
      let anchor = Clock.now clock in
      Ok
        {
          core =
            {
              proto = proto.p;
              state = Dyn (proto.root_state_id, state);
              services = Dyn (proto.root_service_id, services);
              session = inbox.isink.ssession;
              bus = inbox.isink.sbus;
              clock;
              last = anchor;
              next_id = 1;
              depth = 0;
              executions = 0;
              bugs = 0;
              listen = listeners_of [];
              open_ = [];
              special = false;
              special_at = -1;
              special_phase = Clock;
              special_mount = 0;
              fault = None;
              fault_location = None;
            };
          invariants = [];
          sproto_t = proto;
          owner = None;
        }

  let create_exn ?clock proto ~state ~services inbox =
    match create ?clock proto ~state ~services inbox with
    | Ok s -> s
    | Error _ -> invalid_arg "causet: the Event bus was made for another Protocol"

  (* A Store for [proto] with a bus of its own, and the bus's sink. The bus
     is made for [proto], so the Store cannot refuse it. *)
  let open_protocol ?clock proto ~state ~services =
    let sink, inbox =
      Event_bus.create proto ~worker_capacity:Event_bus.default_worker_capacity
    in
    (create_exn ?clock proto ~state ~services inbox, sink)

  (* A Store whose root component is [root]: registers its tree, freezes it
     and opens a Store on it, or returns the registration's problems. *)
  let start ?clock root ~state ~services =
    let registry = Registry.create () in
    Registry.root registry root;
    Result.map
      (fun proto -> open_protocol ?clock proto ~state ~services)
      (Registry.freeze registry)

  let start_exn ?clock root ~state ~services =
    match start ?clock root ~state ~services with
    | Ok started -> started
    | Error problems ->
        invalid_arg
          ("causet: the registration has problems: "
          ^ String.concat "; " (List.map problem_message problems))

  (* The mount at [path], resolved with this Store's Protocol. *)
  let mount store path = Protocol.resolve store.sproto_t path
  let mount_exn store path = Protocol.resolve_exn store.sproto_t path

  (* The Protocol this Store runs, for a Store that replaces it. *)
  let protocol store = store.sproto_t
  let state store = unwrap store.sproto_t.root_state_id store.core.state
  let services store = unwrap store.sproto_t.root_service_id store.core.services
  let readiness store = store.core.bus

  let add_observer store o =
    store.core.listen <- listeners_of (store.core.listen.all @ [ o ])

  let clear_observers store = store.core.listen <- listeners_of []
  let executions store = store.core.executions
  let fault store = store.core.fault
  let fault_location store = store.core.fault_location
  let session store = store.core.session

  (* Closes the bus: queued Events are discarded, later sends fail with
     [Closed] on every thread. A fault does the same. *)
  let close store = close_bus store.core.bus

  let check_invariants store invariants step =
    if (not (List.is_empty invariants)) && Option.is_none store.core.fault then
      let core = store.core in
      let s = state store in
      let at =
        match step with
        | Settled_step { execution; _ } -> Some execution
        | Added | Restored -> None
      in
      let root = core.proto.mounts.(0) in
      let deliver phase (p : Bug.pending) =
        let place =
          {
            Bug_place.phase;
            at;
            component = root.instance;
            path = root.path;
            sequence = core.bugs;
          }
        in
        core.bugs <- core.bugs + 1;
        let report = Bug.into_report p (Some place) in
        notify_bug core report
      in
      Bug.scope (fun () ->
          List.iter
            (fun inv ->
              if not inv.disabled then (
                let outcome = try Ok (inv.check s step) with e -> Error e in
                List.iter (deliver (Invariant inv.iname)) (Bug.take ());
                let failed =
                  match outcome with
                  | Ok (Ok ()) -> None
                  | Ok (Error message) -> Some (Invariant inv.iname, message)
                  | Error e ->
                      inv.disabled <- true;
                      Some
                        ( Invariant_panic inv.iname,
                          "the checker panicked and is disabled: " ^ exn_message e )
                in
                Option.iter
                  (fun (phase, message) ->
                    deliver phase
                      {
                        Bug.message;
                        fields = [ ("invariant", inv.iname) ];
                        file = inv.ifile;
                        line = inv.iline;
                        tag = None;
                      })
                  failed))
            invariants)

  (* Registers an invariant: [check] says what is wrong with the whole State,
     if anything. It runs now ([Added]), then after every root input that
     settles. A violation, or a raise of the checker, is a bug report; the
     Store goes on. *)
  let add_invariant ?(pos = ("", 0, 0, 0)) store name check =
    let file, line, _, _ = pos in
    let inv = { iname = name; ifile = file; iline = line; check; disabled = false } in
    store.invariants <- store.invariants @ [ inv ];
    check_invariants store [ inv ] Added

  (* Takes the invariants out, with their memory, for the Store that replaces
     this one after a restart. *)
  let take_invariants store =
    let taken = store.invariants in
    store.invariants <- [];
    taken

  (* Moves invariants taken from a previous Store into this one and runs each
     on the restored State ([Restored]), before any root input. Those
     disabled there stay disabled. *)
  let restore_invariants store invariants =
    store.invariants <- store.invariants @ invariants;
    check_invariants store invariants Restored

  let settled store execution time category mount kind =
    if not (List.is_empty store.invariants) then
      let path = store.core.proto.mounts.(mount).path in
      check_invariants store store.invariants
        (Settled_step { execution; category; path; kind; time })

  (* The bus learns which thread runs the Store, so that its sends go to the
     Store lane; told again only when that thread changes. *)
  let enter store =
    match store.core.fault with
    | Some f -> Error (Faulted f)
    | None ->
        let thread = Bug.thread_key () in
        (match store.owner with
        | Some t when Bug.same_thread t thread -> ()
        | _ ->
            with_bus store.core.bus (fun l -> l.owner <- Some thread);
            store.owner <- Some thread);
        Ok ()

  (* The root input ended with an exception, or after its fault: the fault is
     recorded unless it was already, where an Effect's dispatch let the
     exception go (the first one wins), and what the root input left open is
     closed. *)
  let faulted store exn bt =
    let core = store.core in
    record_fault core exn bt;
    core.depth <- 0;
    core.open_ <- [];
    core.special <- false;
    Faulted (Option.value core.fault ~default:"")

  (* A root input that returned although its fault was recorded. *)
  let returned_faulted store = faulted store Fault_unwind (Printexc.get_callstack 0)

  (* Dispatches an Action to [mount] as a root input: reads the clock, checks
     admission, and runs it with everything it causes before returning. An
     exception anywhere in that work faults the Store. *)
  let dispatch_to store mount action =
    if mount.mprotocol <> store.core.proto.pid then Error Foreign_mount
    else
      match enter store with
      | Error e -> Error e
      | Ok () ->
          Bug.scope (fun () ->
              let core = store.core in
              let payload = P.payload mount.mcomp.actions action in
              match
                let now = clock_time core in
                match admit core mount.index payload Root None now with
                | None -> `Refused
                | Some st ->
                    `Admitted
                      ( st,
                        now,
                        if not (List.is_empty store.invariants) then P.kind payload
                        else "" )
              with
              | exception e -> Error (faulted store e (Printexc.get_raw_backtrace ()))
              | `Refused -> Ok Disabled
              | `Admitted (st, now, kind) -> (
                  match run_action core mount.index st payload None (Some now) with
                  | exception e -> Error (faulted store e (Printexc.get_raw_backtrace ()))
                  | _ when Option.is_some core.fault -> Error (returned_faulted store)
                  | execution ->
                      settled store execution now Meta.Action mount.index kind;
                      Ok Executed))

  let dispatch_exn store mount action =
    match dispatch_to store mount action with
    | Ok o -> o
    | Error (Faulted m) -> failwith ("causet: the Store faulted: " ^ m)
    | Error Foreign_mount -> invalid_arg "causet: a Mount of another Protocol"
    | Error Foreign_bus -> invalid_arg "causet: a bus of another Protocol"

  (* Delivers up to [max] Events, Store lane first. Each Event is a root
     input execution with its own catch. *)
  let drain_events store max =
    match enter store with
    | Error e -> Error e
    | Ok () ->
        Bug.scope (fun () ->
            let core = store.core in
            let bus_report =
              let os = core.listen.on_bus in
              if Array.length os = 0 then Ok ()
              else
                let depths =
                  with_bus core.bus (fun l ->
                      let d =
                        {
                          Bus_depths.store_lane =
                            List.length l.store_front + Queue.length l.store;
                          worker_lane = worker_length l;
                          rejected_full = l.rejected_full;
                        }
                      in
                      l.rejected_full <- 0;
                      d)
                in
                match
                  observe core os (-1) 0 Bus (fun (o : Observe.t) d -> o.bus d) depths
                with
                | () -> Ok ()
                | exception e -> Error (faulted store e (Printexc.get_raw_backtrace ()))
            in
            match bus_report with
            | Error e -> Error e
            | Ok () ->
                let rec loop delivered =
                  if delivered >= max then Ok delivered
                  else
                    match pop core.bus with
                    | None -> Ok delivered
                    | Some env -> (
                        match run_event core env with
                        | exception e ->
                            Error (faulted store e (Printexc.get_raw_backtrace ()))
                        | _ when Option.is_some core.fault ->
                            Error (returned_faulted store)
                        | execution, time, mount, kind ->
                            settled store execution time Meta.Event mount kind;
                            loop (delivered + 1))
                in
                loop 0)

  (* The sending side of this Store's bus. *)
  let sink store =
    { ssession = store.core.session; sproto = store.core.proto; sbus = store.core.bus }
end
