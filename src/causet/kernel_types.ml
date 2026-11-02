(* Shared erased and typed representations. Keeping the recursive types
   together lets the runtime modules share identities without casts or
   a second set of representations. Public interfaces remain in kernel.mli. *)

(* The causet kernel: components, typed child slots, inline and queued child
   calls, Output lowering, parent admission guards, mount paths,
   per-execution request counters, the root-boundary fault policy, Effects
   with scoped Services, synchronous Effect children, Events delivered by
   [drain_events] through typed reply routes and a two-lane bus, bug reports,
   invariants and the component test harness.

   State is immutable: a reducer reads its State and sets a new one, and a
   child's new State is written back into its parent's through the slot's
   lens. The order "change State, then emit" is checked as the reducer runs:
   a write after the first emission, or any use of a context after its
   handler returned, raises, which faults the Store. *)

open Ids
open Observe
module P = Message

(* A value of some type, with the witness to get it back. *)
type dyn = Dyn : 'a Type.Id.t * 'a -> dyn

let unwrap : type a. a Type.Id.t -> dyn -> a =
 fun id (Dyn (id', v)) ->
  match Type.Id.provably_equal id id' with
  | Some Type.Equal -> v
  | None -> invalid_arg "causet: a value of another type"

(* A pair of accessors from a parent value to a child value. [get] may find
   nothing: a child mounted on an optional field that is absent. *)
type ('p, 'c) lens = { get : 'p -> 'c option; set : 'p -> 'c -> 'p }

(* What an inline child call did. *)
type outcome = Reduced | Refused

(* What a dispatch did. [Faulted] only from an Effect's dispatch, after its
   handler caught the exception of an earlier one: nothing ran. *)
type dispatch_outcome = Executed | Disabled | Faulted

(* Why a Store call did nothing. *)
type dispatch_error =
  | Foreign_mount  (** The Mount was resolved against another Protocol. *)
  | Foreign_bus  (** The Event bus was made for another Protocol. *)
  | Faulted of string  (** The Store faulted, now or earlier, with this message. *)

(* An observer's exception inside a reducer, unwinding with its place: the
   fault is placed there only if this very exception ends the root input, or
   leaves an Effect's dispatch. *)
exception
  Observer_panic of {
    at : execution_id;
    boundary : boundary;
    mount : int;
    exn : exn;
    location : string option;  (** Where the observer raised it. *)
  }

(* What a root input unwinds with once its fault is recorded and a handler
   that caught the exception has returned: the rest of its work does not
   run. *)
exception Fault_unwind

(* Files of the standard library and of test frameworks: where [failwith],
   [invalid_arg] or a failed check raise, not where the code that called
   them is. *)
let library_frame filename =
  let base = Filename.basename filename in
  let stdlib =
    [
      "stdlib.ml";
      "list.ml";
      "array.ml";
      "option.ml";
      "result.ml";
      "fun.ml";
      "printf.ml";
      "format.ml";
      "camlinternalFormat.ml";
      "hashtbl.ml";
      "map.ml";
      "set.ml";
      "queue.ml";
      "stack.ml";
      "string.ml";
      "bytes.ml";
      "seq.ml";
      "lazy.ml";
      "camlinternalLazy.ml";
      "printexc.ml";
      "mutex.ml";
      "dynarray.ml";
    ]
  in
  List.mem base stdlib
  || String.starts_with ~prefix:"stdlib__" base
  ||
  let needle = "alcotest" in
  let n = String.length needle and m = String.length filename in
  let rec has i = i + n <= m && (String.sub filename i n = needle || has (i + 1)) in
  has 0

(* Where a backtrace's exception was raised: the first frame with a
   location outside the standard library and test frameworks, as
   [file:line]; the first frame with any location when there is none. *)
let raise_location bt =
  match Printexc.backtrace_slots bt with
  | None -> None
  | Some slots -> (
      let located = Array.to_list slots |> List.filter_map Printexc.Slot.location in
      let at (l : Printexc.location) = Printf.sprintf "%s:%d" l.filename l.line_number in
      match
        List.find_opt
          (fun (l : Printexc.location) -> not (library_frame l.filename))
          located
      with
      | Some l -> Some (at l)
      | None -> Option.map at (List.nth_opt located 0))

let exn_message = function
  | Failure s | Invalid_argument s -> s
  | Assert_failure (file, line, _) -> Printf.sprintf "assertion failed at %s:%d" file line
  | Stack_overflow -> "stack overflow"
  | Out_of_memory -> "out of memory"
  | e -> Printexc.to_string e

(* One registration problem. Paths are mount paths. *)
type problem =
  | Root_registered_twice
  | No_root
  | Root_has_outputs  (** The root reports Outputs, but it has no parent to lower them. *)
  | Slot_bound_twice of string  (** Two slots of one parent share a name. *)
  | No_lowerer of string  (** The child reports Outputs and the slot binds no lowerer. *)
  | Event_registered_twice of { path : string; event : string }
  | Instance_collision of { first : string; second : string }
  | No_service_projection of { path : string; at : string }
      (** A component with Effects whose Service no lens reaches at [at]. *)
  | Root_guard_no_mount of string  (** A root guard's path names no mount. *)
  | Root_guard_other_component of string
      (** A root guard's path starts at another root ([/]), or reaches a mount of another
          component than its type says. *)
  | Root_guard_on_root
      (** A root guard on the root itself, whose own [is_enabled] already reads the whole
          State. *)
  | Root_guard_twice of string  (** Two root guards on one mount. *)

let problem_message = function
  | Root_registered_twice -> "root registered twice"
  | No_root -> "no root component"
  | Root_has_outputs -> "the root component's Output must be never"
  | Slot_bound_twice path -> Printf.sprintf "slot %s bound twice" path
  | No_lowerer path -> Printf.sprintf "%s reports Outputs but has no lowerer" path
  | Event_registered_twice { path; event } ->
      Printf.sprintf "%s registered Event %s twice" path event
  | Instance_collision { first; second } ->
      Printf.sprintf "InstanceId collision: %s and %s" first second
  | No_service_projection { path; at } ->
      Printf.sprintf "%s has Effects but no Service projection reaches it at %s" path at
  | Root_guard_no_mount path -> Printf.sprintf "root guard on %s: no such mount" path
  | Root_guard_other_component path ->
      Printf.sprintf "root guard on %s: the mount holds another component type" path
  | Root_guard_on_root -> "root guard on /: use the root's own is_enabled"
  | Root_guard_twice path -> Printf.sprintf "two root guards on %s" path

(* ------------------------------------------------------------------------ *)
(* The erased core *)

type queued = {
  q_category : Meta.category;
  q_target : int;
  q_payload : P.payload;
  q_origin : Meta.origin;
}

(* Per-execution scratch: the private FIFO and the counters an inline child
   shares with its parent. *)
type exec = {
  x_session : session_id;
  x_execution : execution_id;
  mutable next_emission : int;
  mutable next_request : int;
  fifo : queued Queue.t;
}

let new_exec session execution =
  {
    x_session = session;
    x_execution = execution;
    next_emission = 0;
    next_request = 0;
    fifo = Queue.create ();
  }

let claim_emission exec =
  let ordinal = exec.next_emission in
  exec.next_emission <- ordinal + 1;
  ordinal

type envelope = {
  e_session : session_id;
  e_instance : instance_id;
  e_event : int;
  e_kind : string;
  e_effect : execution_id option;
  e_payload : P.payload;
}

type lane = Store_lane | Worker_lane
type held_entry = { h_id : int; h_lane : lane; h_front : bool; h_envelope : envelope }

type held = {
  mutable next_held : int;
  mutable waiting : held_entry list (* in send order *);
}

type readiness_target = { mutable wake : (unit -> unit) option }

module Readiness_status = struct
  type t = Events | Closed
end

type lanes = {
  mutable owner : (int * int) option;  (** The thread that runs the Store. *)
  mutable store_front : envelope list;  (** Sent to the head of the Store lane. *)
  store : envelope Queue.t;
  mutable worker_front : envelope list;  (** Sent to the head of the worker lane. *)
  worker : envelope Queue.t;
  worker_capacity : int;
  mutable closed : bool;
  mutable rejected_full : int;
  held : held option;  (** On a held bus, sends wait here until released. *)
  mutable readiness_target : readiness_target option;
}

type bus = { lock : Mutex.t; lanes : lanes }
type erased_lens = { lget : dyn -> dyn option; lset : dyn -> dyn -> dyn }

type event_binding = {
  ev_uid : int;
  ev_name : string;
  adapt : exec -> int -> instance_id -> P.payload -> unit;
}

type mount_def = {
  path : string;
  instance : instance_id;
  parent : int option;
  mutable chain : int array;  (** The mounts from the root's child down to this one. *)
  comp_uid : int;
  has_effects : bool;
  mutable children : (string * int) list;
  lens : erased_lens option;
  service : (dyn -> dyn) option;
  guard : (dyn -> P.payload -> timestamp -> bool) option;
  root_guard : (dyn -> dyn -> P.payload -> timestamp -> bool) option;
      (** The application's guard on this mount ([Registry.admit]), set at freeze: it
          reads the whole State, then the mount's own. *)
  lower : (exec -> int -> Meta.origin -> P.payload -> unit) option;
  mutable events : event_binding list;
  run : core -> int -> exec -> dyn -> P.payload -> Meta.t -> unit;
  handle : core -> int -> P.payload -> Meta.t -> unit;
  admit : dyn -> P.payload -> timestamp -> bool;
}

and protocol = {
  pid : int;
  mounts : mount_def array;
  by_instance : (instance_id, int) Hashtbl.t;
}

(* The observers installed and, for each callback, those that have one of
   their own: a callback left as [Observe.none]'s is never called, and a
   boundary none of them has a callback for is skipped whole, with no
   record made. *)
and listeners = {
  all : Observe.t list;
  on_action_start : Observe.t array;
  on_action_finish : Observe.t array;
  on_action_settled : Observe.t array;
  on_effect_start : Observe.t array;
  on_effect_finish : Observe.t array;
  on_event_start : Observe.t array;
  on_event_finish : Observe.t array;
  on_event_settled : Observe.t array;
  on_action_refused : Observe.t array;
  on_inline_action : Observe.t array;
  on_fault : Observe.t array;
  on_bus : Observe.t array;
  on_bug : Observe.t array;
}

(* The non-generic execution core. *)
and core = {
  proto : protocol;
  mutable state : dyn;
  services : dyn;
  session : session_id;
  bus : bus;
  clock : Clock.t;
  mutable last : timestamp;  (** The latest execution time. *)
  mutable next_id : int;  (** The next execution's ID: 1 before the first. *)
  mutable depth : int;
  mutable executions : int;
  mutable bugs : int;
  mutable listen : listeners;
  mutable open_ : (execution_id option * phase * int) list;
      (** Active executions and what they are doing, innermost first. *)
  mutable special : bool;
      (** Whether a phase that runs no application code runs (the clock, observers): then
          [special_phase], within execution [special_at] ([-1] for none), on mount
          [special_mount]. Fields, so that entering one allocates nothing. *)
  mutable special_at : int;
  mutable special_phase : phase;
  mutable special_mount : int;
  mutable fault : string option;
      (** The fault's message, once the Store has faulted. It is recorded where the
          exception first leaves the work an Effect's dispatch ran, or at the root-input
          boundary: the first one wins. *)
  mutable fault_location : string option;
      (** Where the exception that faulted it was raised, when backtraces are recorded
          ([Printexc.record_backtrace true]). *)
}

(* ------------------------------------------------------------------------ *)
(* Components and their contexts *)

type ctx_phase = Mutating | Emitting | Finished

(* A reusable state machine: its State, its Service, the messages it
   handles, and the functions that handle them. *)
type ('s, 'svc, 'a, 'e, 'o) component = {
  uid : int;
  name : string;
  state_id : 's Type.Id.t;
  service_id : 'svc Type.Id.t;
  actions : 'a P.t;
  effects : 'e P.t;
  outputs : 'o P.t;
  is_enabled : 's -> 'a -> timestamp -> bool;
  reduce : ('s, 'svc, 'a, 'e, 'o) ctx -> 'a -> Meta.t -> unit;
  handle : ('s, 'svc, 'a, 'e, 'o) effect_ctx -> 'e -> Meta.t -> unit;
  register : ('s, 'svc, 'a, 'e, 'o) registrar -> unit;
}

(* A reducer's context: read and set the component's State, make request
   IDs, call children inline; then emit. *)
and ('s, 'svc, 'a, 'e, 'o) ctx = {
  mutable cstate : 's;
  mutable cphase : ctx_phase;
  cmeta : Meta.t;
  ccomp : ('s, 'svc, 'a, 'e, 'o) component;
  backend : backend;
  cmount : int;
}

and backend = In_store of core * exec | In_harness of collected

and collected = {
  c_session : session_id;
  c_execution : execution_id;
  mutable c_next_request : int;
  c_emissions : raw_emission Queue.t;
}

and raw_emission =
  | Raw_action of P.payload
  | Raw_effect of P.payload
  | Raw_output of P.payload
  | Raw_inline of child_call
  | Raw_to_child of child_call

(* A call to a child, recorded rather than run: the child is not mounted
   under a Harness. *)
and child_call = { slot : string; call_kind : string; call : P.payload }

(* What an Effect handler gets: read-only State, its Service, synchronous
   Action dispatch and reply routes. *)
and ('s, 'svc, 'a, 'e, 'o) effect_ctx = {
  ecore : core;
  emount : int;
  emeta : Meta.t;
  mutable e_next_emission : int;
  ecomp : ('s, 'svc, 'a, 'e, 'o) component;
  mutable evalid : bool;
}

(* What a component's [register] declares through. *)
and ('s, 'svc, 'a, 'e, 'o) registrar = {
  builder : builder;
  rmount : int;
  rcomp : ('s, 'svc, 'a, 'e, 'o) component;
  mutable rlive : bool;
      (** Whether its [register] still runs: a registrar kept for later raises rather than
          change a tree that may be frozen and in use. *)
}

and builder = { bmounts : mount_def Dynarray.t; mutable errors : problem list }

(* An Action-only port, given to an Output lowerer (scoped to the parent) or
   an Event adapter (scoped to its own mount). *)
type 'a port = { pexec : exec; ptarget : int; pmode : port_mode; paction : 'a P.t }

and port_mode =
  | Lowering of { origin : Meta.origin; mutable next : int }
  | Adapting of instance_id

(* A child slot declared by the parent whose types are ['p], holding a
   component. *)
type ('p, 'cs, 'csvc, 'ca, 'ce, 'co) child = {
  slot_name : string;
  slot_comp : ('cs, 'csvc, 'ca, 'ce, 'co) component;
}

(* The only external handle type; valid on every Store of its Protocol. *)
type ('s, 'svc, 'a, 'e, 'o) mount = {
  index : int;
  mprotocol : int;
  minstance : instance_id;
  mcomp : ('s, 'svc, 'a, 'e, 'o) component;
}

(* A typed path built from slots, resolved once against a Protocol. *)
type ('s, 'svc, 'a, 'e, 'o) path = {
  names : string list;  (** innermost first *)
  pcomp : ('s, 'svc, 'a, 'e, 'o) component;
  proot : int;  (** The uid of the component the path starts at. *)
}

let next_uid = Atomic.make 1

let component ~name ~actions ~effects ~outputs ?(is_enabled = fun _ _ _ -> true) ~reduce
    ?(handle = fun _ _ _ -> failwith "causet: this component handles no Effects")
    ?(register = ignore) () =
  {
    uid = Atomic.fetch_and_add next_uid 1;
    name;
    state_id = Type.Id.make ();
    service_id = Type.Id.make ();
    actions;
    effects;
    outputs;
    is_enabled;
    reduce;
    handle;
    register;
  }
