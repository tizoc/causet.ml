(** The kernel: components, the Protocol they are frozen into, the Store that runs them,
    the Event bus and the reducer-level test harness. Everything here is re-exported by
    {!Causet}. *)

open Ids

(** {1 Values} *)

type ('p, 'c) lens = { get : 'p -> 'c option; set : 'p -> 'c -> 'p }
(** A pair of accessors from a parent's State to a child's. [get] may find nothing: a
    child mounted on an optional field that is absent. *)

(** What an inline child call did. *)
type outcome =
  | Reduced  (** The child's reducer ran. *)
  | Refused  (** Its admission refused the Action: nothing ran. *)

(** What a dispatch did. *)
type dispatch_outcome =
  | Executed  (** The Action ran, with everything it caused. *)
  | Disabled  (** Its admission refused it: nothing ran. *)
  | Faulted
      (** The Store faulted earlier in this root input: nothing ran. Only an Effect's
          [dispatch] returns it, after its handler caught the exception of an earlier
          dispatch; a Store call returns [Error (Faulted message)]. *)

(** Why a Store call did nothing. *)
type dispatch_error =
  | Foreign_mount  (** The Mount was resolved against another Protocol. *)
  | Foreign_bus  (** The Event bus was made for another Protocol. *)
  | Faulted of string
      (** The Store has faulted; this is the fault's message. Its bus closed as it did. *)

exception
  Observer_panic of {
    at : execution_id;
    boundary : Observe.boundary;
    mount : int;
    exn : exn;
    location : string option;
  }
(** An observer's raise inside a reducer, wrapped with its place, so that a reducer that
    catches it cannot mistake it for its own. *)

val exn_message : exn -> string
(** A fault's message for an exception: its text, or its [Printexc] form. *)

val raise_location : Printexc.raw_backtrace -> string option
(** Where an exception was raised, as [file:line], from its backtrace: the first frame
    outside causet's own code and the standard library, or [None] when backtraces are not
    recorded. *)

(** What [Registry.freeze] refuses. *)
type problem =
  | Root_registered_twice
  | No_root
  | Root_has_outputs
  | Slot_bound_twice of string
  | No_lowerer of string
  | Event_registered_twice of { path : string; event : string }
  | Instance_collision of { first : string; second : string }
  | No_service_projection of { path : string; at : string }
  | Root_guard_no_mount of string
      (** A root guard's path ({!Registry.admit}) names no mount: the whole path. *)
  | Root_guard_other_component of string
      (** A root guard's path starts at another component than the Registry's root (the
          path is then [/]), or reaches a mount of another component than its type says.
      *)
  | Root_guard_on_root
      (** A root guard on the root itself, whose own [is_enabled] already reads the whole
          State. *)
  | Root_guard_twice of string  (** Two root guards on one mount. *)

val problem_message : problem -> string

(** {1 Components} *)

type ('s, 'svc, 'a, 'e, 'o) component
(** A component with State ['s], Service ['svc], Actions ['a], Effects ['e] and Outputs
    ['o]. *)

type ('s, 'svc, 'a, 'e, 'o) ctx
(** A reducer's context: its State, then its emissions. *)

type ('s, 'svc, 'a, 'e, 'o) effect_ctx
(** An Effect handler's context: read-only State, its Service, synchronous dispatch and
    reply routes. *)

type ('s, 'svc, 'a, 'e, 'o) registrar
(** What a component's [register] declares through. *)

type 'a port
(** What a lowerer or an Event adapter emits Actions of type ['a] through. *)

type (+'p, 'cs, 'csvc, 'ca, 'ce, 'co) child
(** A slot of parent ['p] (its five types, as a tuple) holding a child component of the
    other five. ['p] is covariant so that a slot made by {!val-child} at the top level
    stays polymorphic in it until its parent's [register] fixes it. *)

type ('s, 'svc, 'a, 'e, 'o) mount
(** A component's place in a frozen Protocol, resolved from a {!path}. *)

type ('s, 'svc, 'a, 'e, 'o) path
(** A component's path from the root, through slot names. *)

val component :
  name:string ->
  actions:'a Message.t ->
  effects:'e Message.t ->
  outputs:'o Message.t ->
  ?is_enabled:('s -> 'a -> timestamp -> bool) ->
  reduce:(('s, 'svc, 'a, 'e, 'o) ctx -> 'a -> Meta.t -> unit) ->
  ?handle:(('s, 'svc, 'a, 'e, 'o) effect_ctx -> 'e -> Meta.t -> unit) ->
  ?register:(('s, 'svc, 'a, 'e, 'o) registrar -> unit) ->
  unit ->
  ('s, 'svc, 'a, 'e, 'o) component
(** A component. [is_enabled] is its own admission check, [true] by default; [reduce] runs
    for each admitted Action; [handle] runs its Effects; [register] declares its children
    and the Events it adapts. *)

val component_name : (_, _, _, _, _) component -> string

val child :
  string -> ('cs, 'csvc, 'ca, 'ce, 'co) component -> ('p, 'cs, 'csvc, 'ca, 'ce, 'co) child
(** A slot named [name] holding [comp]: the child's path is its parent's plus [/name]. *)

val child_name : (_, _, _, _, _, _) child -> string

(** {1 Reducers} *)

module Ctx : sig
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) ctx

  val state : ('s, _, _, _, _) ctx -> 's
  (** The component's State. *)

  val set_state : ('s, _, _, _, _) ctx -> 's -> unit
  (** Sets the component's State; only before the first emission. In a Store it reaches
      the Store's State when the reducer returns, or raises: a raise after it keeps it,
      since a fault rolls nothing back. *)

  val update : ('s, _, _, _, _) ctx -> ('s -> 's) -> unit

  val meta : _ ctx -> Meta.t
  (** This execution's metadata. *)

  val new_request : _ ctx -> Request_id.t
  (** A request ID unique in this execution, shared with children called inline; only
      before the first emission. *)

  val reduce_child :
    ('s, 'svc, 'a, 'e, 'o) ctx ->
    ('s * 'svc * 'a * 'e * 'o, 'cs, 'csvc, 'ca, 'ce, 'co) child ->
    'ca ->
    outcome
  (** Runs the child's admission and reducer inside this execution, and writes its new
      State back into this component's. It makes no execution of its own; it is how a
      parent learns of a refusal. Only before the first emission.

      The child's checks run first, in a Store's order: its State is there, its root guard
      (only when this is the root's context, the one that holds the whole State; see
      {!Registry.admit}), the slot guard, its own [is_enabled]. An exception raised in
      them, or in the child's reducer, propagates through this call to the reducer that
      made it, like any exception there; uncaught, it faults the Store. *)

  val emit_action : (_, _, 'a, _, _) ctx -> 'a -> unit
  (** Queues an Action for this component, admitted when its turn comes. From here on the
      State is read-only. *)

  val emit_effect : (_, _, _, 'e, _) ctx -> 'e -> unit
  (** Queues an Effect for this component's handler. *)

  val to_child :
    ('s, 'svc, 'a, 'e, 'o) ctx ->
    ('s * 'svc * 'a * 'e * 'o, 'cs, 'csvc, 'ca, 'ce, 'co) child ->
    'ca ->
    unit
  (** Queues an Action for a child, admitted when its turn comes. *)

  val emit_output : (_, _, _, _, 'o) ctx -> 'o -> unit
  (** Reports an Output to the parent, whose lowerer turns it into Actions. *)
end

module Port : sig
  type 'a t = 'a port

  val emit_action : 'a port -> 'a -> unit
end

(** {1 The Event bus} *)

(** Why an Event was not sent; it is handed back. *)
type 'ev send_error =
  | Full of 'ev  (** The worker lane is at its capacity. *)
  | Closed of 'ev  (** The Store is gone: closed, or faulted. *)
  | Wrong_protocol of 'ev  (** The Mount belongs to another Protocol. *)
  | Unregistered of 'ev  (** The mount does not adapt this Event type. *)

type sink
(** The sending side of a Store's bus, for any thread. A send from the thread that runs
    the Store goes to the Store lane, which is unbounded; one from any other thread goes
    to the worker lane, which holds at most the bus's [worker_capacity] Events. The Store
    drains the Store lane first. A front send ({!Reply.send_front}) goes to the head of
    the lane it takes; each lane runs its front sends, newest first, before the others,
    oldest first. Once the Store is closed or faulted, every send fails with [Closed],
    from any thread. *)

type inbox
(** The receiving side, given to the Store. *)

(** Synchronous host notifications, independent of an event-loop library. *)
module Readiness : sig
  type t
  type status = Events | Closed
  type error = Already_subscribed
  type subscription

  val current : t -> status option
  (** [None] while open and empty. Held Events count only after release. Reading readiness
      never consumes or reserves input. *)

  val subscribe : t -> (unit -> unit) -> (subscription, error) result
  (** Installs the bus's one persistent driver notification. It runs immediately if
      already ready, when an empty bus gains deliverable Events, and on closure. Bursts
      coalesce. Checking and registration share the queue lock. The callback runs outside
      it, on the notifying thread, and must only signal the driver; it must not run the
      Store. Notification exceptions are ignored. A closed bus notifies immediately and
      retains no callback. *)

  val cancel : subscription -> unit
  (** Unregisters the callback and releases its captures; idempotent. An already-issued
      notification may still run. Queued input is unaffected. *)

  (** A reusable native wait handle. It owns the bus's wake registration and a socket
      pair; create it once and close it on the driver thread when done. *)
  module Blocking : sig
    type waiter

    val create : t -> (waiter, error) result
    (** Takes the bus's one registration, the one {!subscribe} takes, and opens the
        waiter's socket pair. It fails with [Already_subscribed] while a subscription or
        another waiter holds the registration, and while this waiter holds it,
        {!subscribe} fails the same way. A closed bus keeps no registration: creating a
        waiter on it succeeds, and its waits return [Closed] at once. Native socket
        wakeups work on Unix and Windows without a descriptor-number limit. *)

    val wait_timeout : waiter -> timeout:float -> status option
    (** Blocks a dedicated driver thread until Events, closure or the timeout in seconds.
        [None] means timeout. Neither waiting nor waking consumes input. No polling or
        per-wait socket allocation is needed. Waiting and closing belong to the same
        driver. A timeout that is negative, infinite or NaN raises [Invalid_argument], and
        so does a wait on a closed waiter. *)

    val close : waiter -> unit
    (** Cancels the registration and closes both sockets; idempotent. *)
  end
end

module Inbox : sig
  val readiness : inbox -> Readiness.t
  (** A handle kept by the host after the Store takes the inbox. *)
end

type 'ev reply
(** A typed reply route for the Effect that made it: a Service keeps it and sends its
    answer from any thread. *)

module Reply : sig
  type 'ev t = 'ev reply

  val instance : _ reply -> instance_id

  val answers : _ reply -> execution_id
  (** The Effect it answers. *)

  val session : _ reply -> session_id

  val try_send : 'ev reply -> 'ev -> (unit, 'ev send_error) result
  (** Sends now: on the Store's thread to the Store lane, else to the worker lane, refused
      when that is full. *)

  val try_send_as_worker : 'ev reply -> 'ev -> (unit, 'ev send_error) result
  (** As from a worker thread, whatever thread it is on. *)

  val send_front : 'ev reply -> 'ev -> (unit, 'ev send_error) result
  (** To the head of its lane: on the Store's thread the Store lane's, delivered before
      everything waiting; from any other thread the worker lane's, delivered before the
      rest of that lane, and refused with [Full] when it is at capacity, never waiting. *)

  val send_timeout : 'ev reply -> 'ev -> timeout:float -> (unit, 'ev send_error) result
  (** Waits up to [timeout] seconds for room on a full worker lane. *)
end

module Sink : sig
  type t = sink

  val session : sink -> session_id

  val try_send_to :
    sink -> (_, _, _, _, _) mount -> 'ev Message.t -> 'ev -> (unit, 'ev send_error) result
  (** Sends [ev], of Event type [msg], to [mount]: from the Store's thread to the Store
      lane, else to the worker lane. *)

  val try_send_to_as_worker :
    sink -> (_, _, _, _, _) mount -> 'ev Message.t -> 'ev -> (unit, 'ev send_error) result

  val send_timeout_to :
    sink ->
    (_, _, _, _, _) mount ->
    'ev Message.t ->
    'ev ->
    timeout:float ->
    (unit, 'ev send_error) result

  val reply_for :
    sink ->
    (_, _, _, _, _) mount ->
    'ev Message.t ->
    answers:execution_id ->
    'ev reply option
  (** A reply route to [mount] as if made by the Effect [answers]; [None] when the mount
      does not adapt [msg]. For tests and simulated Services. *)
end

(** The lane an Event is waiting in. *)
type lane = Store_lane | Worker_lane

type held_event = {
  held_id : int;
  held_path : string;
  held_instance : instance_id;
  held_event : string;  (** Its Event type's name. *)
  held_kind : string;
  held_answers : execution_id option;
  held_session : session_id;
  held_lane : lane;
  held_front : bool;  (** Sent to the front of its lane. *)
}
(** An Event a held bus is holding, as a test sees it. *)

type release_error = Not_held | Release_full | Release_closed

(** A held bus's control: every Event sent to it waits here until the test releases it
    into the lanes, or discards it. *)
module Held : sig
  type t

  val list : t -> held_event list
  (** The Events held, in the order they were sent. *)

  val length : t -> int
  val is_empty : t -> bool

  val inspect : t -> int -> 'ev Message.t -> 'ev option
  (** The held Event [id], if it is of type [msg]. *)

  val release : t -> int -> (unit, release_error) result
  (** Moves the held Event into its lane, where the Store drains it. *)

  val discard : t -> int -> bool
  (** Drops it; false when it is not held. *)
end

(** {1 Protocols} *)

module Registrar : sig
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) registrar

  val child :
    ('s, 'svc, 'a, 'e, 'o) registrar ->
    ('s * 'svc * 'a * 'e * 'o, 'cs, 'csvc, 'ca, 'ce, 'co) child ->
    lens:('s, 'cs) lens ->
    ?service:('svc -> 'csvc) ->
    ?guard:('s -> 'ca -> timestamp -> bool) ->
    ?on_output:
      (('s * 'svc * 'a * 'e * 'o, 'cs, 'csvc, 'ca, 'ce, 'co) child ->
      'co ->
      'a port ->
      unit) ->
    unit ->
    unit
  (** Declares the child at [slot], its State projected by [lens], its Service by
      [service]; a [guard] on every route into it, reading this component's State; and the
      lowerer for its Outputs, required when it has any. *)

  val event :
    ('s, 'svc, 'a, 'e, 'o) registrar -> 'ev Message.t -> ('ev -> 'a port -> unit) -> unit
  (** Declares that this component adapts Events of type [msg] into its Actions. *)
end

type ('s, 'svc) protocol_t
(** A frozen Protocol: the mounted tree of components, shared by every Store of it. *)

type ('s, 'svc) registry
(** The tree being built. *)

module Registry : sig
  type ('s, 'svc) t = ('s, 'svc) registry

  val create : unit -> ('s, 'svc) registry

  val root : ('s, 'svc) registry -> ('s, 'svc, 'a, 'e, 'o) component -> unit
  (** The root component, whose State and Service are the whole Store's. *)

  val admit :
    ('s, 'svc) registry ->
    ('ds, _, 'da, _, _) path ->
    ('s -> 'ds -> 'da -> timestamp -> bool) ->
    unit
  (** [admit registry path guard] sets a root guard on the mount at [path], anywhere below
      the root: a check of the whole State, the mount's own State, its Action and the
      time, as pure as a component's [is_enabled]. It is resolved at {!freeze}.

      It checks every Action that enters the mount, that is, that the Store admits there
      to start an execution: a root input, a queued Action, an Action adapted from an
      Event or lowered from an Output, a dispatch from an Effect handler, a
      {!Store.dispatch_to}. It also checks an inline call ({!Ctx.reduce_child}) the root
      makes, the one caller that holds the whole State. An inline call made below the root
      runs inside its caller's execution without it: the caller takes that on, and
      observers are told ([Observe.Inline_call.root_guard_skipped]). So an Action
      forwarded inline below the root is checked where it entered, by that mount's root
      guard.

      The checks run in this order: the mount's State is there, its root guard, its
      parent's slot guard, its own [is_enabled]. An exception raised in the guard faults
      the Store, as one anywhere in a root input does. On the root's inline call it
      propagates through {!Ctx.reduce_child} to the caller first, as one raised in the
      call's other checks or in the child's reducer does, like any exception there:
      uncaught, it faults the Store. {!freeze} reports a path that names no mount
      ([Root_guard_no_mount]) or another component ([Root_guard_other_component]), a root
      guard on the root itself ([Root_guard_on_root]), and a second root guard on a mount
      ([Root_guard_twice]). *)

  val freeze : ('s, 'svc) registry -> (('s, 'svc) protocol_t, problem list) result
  (** Mounts the tree and checks it. *)
end

(** Why a path did not resolve. *)
type resolve_error =
  | No_mount of { name : string; under : string }
      (** No slot [name] under the mount at [under]. *)
  | Other_component of string
      (** The mount at this path holds another component type than the path says, or the
          path starts at another root (the path is then [/]). *)

module Path : sig
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) path

  val root : ('s, 'svc, 'a, 'e, 'o) component -> ('s, 'svc, 'a, 'e, 'o) path

  val child :
    (_, _, _, _, _) path ->
    (_, 'cs, 'csvc, 'ca, 'ce, 'co) child ->
    ('cs, 'csvc, 'ca, 'ce, 'co) path
end

module Protocol : sig
  type ('s, 'svc) t = ('s, 'svc) protocol_t

  val resolve :
    (_, _) protocol_t ->
    ('s, 'svc, 'a, 'e, 'o) path ->
    (('s, 'svc, 'a, 'e, 'o) mount, resolve_error) result
  (** The mount at [path], a handle for every Store of this Protocol. A path that starts
      at another root component than this Protocol's is refused with
      [Other_component "/"], as {!Registry.admit}'s is at {!Registry.freeze}. *)

  val resolve_exn :
    (_, _) protocol_t -> ('s, 'svc, 'a, 'e, 'o) path -> ('s, 'svc, 'a, 'e, 'o) mount

  val path_of : (_, _) protocol_t -> (_, _, _, _, _) mount -> string
  (** The mount's path, such as [/cart]. *)

  val instance_of : (_, _, _, _, _) mount -> instance_id
  (** The mount's identity: the xxh3-64 hash of its path, as in the Rust port. *)
end

module Event_bus : sig
  val default_worker_capacity : int
  (** The worker lane's capacity on the bus {!Store.start} and {!Store.open_protocol}
      make, 1024: how many Events from other threads wait before a send fails. *)

  val create : (_, _) protocol_t -> worker_capacity:int -> sink * inbox
  (** A bus for one Store, in a fresh session. The worker lane holds at most
      [worker_capacity] Events. *)

  val with_session :
    (_, _) protocol_t -> worker_capacity:int -> session_id -> sink * inbox

  val held :
    (_, _) protocol_t -> worker_capacity:int -> session_id -> sink * inbox * Held.t
  (** A bus that holds every Event until the test releases it. *)
end

(** {1 Effect handlers} *)

module Effect_ctx : sig
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) effect_ctx

  val meta : _ effect_ctx -> Meta.t

  val state : ('s, _, _, _, _) effect_ctx -> 's option
  (** The component's State now; [None] when it is absent. *)

  val service : (_, 'svc, _, _, _) effect_ctx -> 'svc

  val dispatch : (_, _, 'a, _, _) effect_ctx -> 'a -> dispatch_outcome
  (** Runs an Action of this component at once, with everything it causes.

      An exception in that work faults the Store as it leaves this call, whether or not
      the handler catches it: the fault is the exception's, with its place. A handler that
      catches it finds the Store faulted: a later [dispatch] runs nothing and returns
      [Faulted], the bus is closed, and as the handler returns, the rest of the root input
      is dropped and the Store call returns the fault. *)

  val reply : _ effect_ctx -> 'ev Message.t -> 'ev reply
  (** A reply route for the work this Effect starts. *)
end

(** {1 Stores} *)

(** When an invariant is checked. *)
type invariant_step =
  | Added  (** As it is added, on the State as it is. *)
  | Restored
      (** As it is moved into a new Store, with its memory, after a restart
          ({!Store.restore_invariants}): the State is the restored one, and no root input
          has run yet. *)
  | Settled_step of {
      execution : execution_id;
      category : Meta.category;
      path : string;
      kind : string;
      time : timestamp;
    }  (** After a root input settled. *)

type ('s, 'svc) store
(** A runtime holding one Protocol's State and Services. *)

type 's invariants
(** A Store's invariants with their memory, taken out to move into the Store that replaces
    it after a restart ({!Store.take_invariants}). *)

module Invariants : sig
  type 's t = 's invariants

  val length : _ invariants -> int
  (** How many there are. *)

  val is_empty : _ invariants -> bool
  (** Whether there are none. *)
end

module Store : sig
  type ('s, 'svc) t = ('s, 'svc) store

  val start :
    ?clock:Clock.t ->
    ('s, 'svc, _, _, _) component ->
    state:'s ->
    services:'svc ->
    (('s, 'svc) store * sink, problem list) result
  (** [start root ~state ~services] is a Store whose root component is [root], starting
      from [state] with [services], and the sink for Events nobody asked for (the host's
      input, or a Service's that listens; an answer to an Effect goes through its reply):
      [root]'s tree is registered and frozen, and the Store drains a bus of its own (a
      worker lane of {!Event_bus.default_worker_capacity} Events, a fresh session). The
      clock defaults to the system's. Fails with the registration's problems, as
      {!Registry.freeze} reports them. For root guards or several Stores on one Protocol,
      freeze a {!Registry} and use {!open_protocol}; for a chosen session or a held bus,
      {!Event_bus} and {!create}. *)

  val start_exn :
    ?clock:Clock.t ->
    ('s, 'svc, _, _, _) component ->
    state:'s ->
    services:'svc ->
    ('s, 'svc) store * sink
  (** [start], raising [Invalid_argument] with every problem in words
      ({!problem_message}). *)

  val open_protocol :
    ?clock:Clock.t ->
    ('s, 'svc) protocol_t ->
    state:'s ->
    services:'svc ->
    ('s, 'svc) store * sink
  (** A Store for [protocol], starting from [state] with [services], and the sink for the
      Events nobody asked for. The Store drains a bus of its own, as {!start}'s does; the
      bus is made for [protocol], so this cannot fail. ([open] is a keyword.) *)

  val mount :
    (_, _) store ->
    ('s, 'svc, 'a, 'e, 'o) path ->
    (('s, 'svc, 'a, 'e, 'o) mount, resolve_error) result
  (** The mount at [path], resolved with this Store's Protocol ({!Protocol.resolve}): a
      handle for {!dispatch_to} and the Event sink. *)

  val mount_exn :
    (_, _) store -> ('s, 'svc, 'a, 'e, 'o) path -> ('s, 'svc, 'a, 'e, 'o) mount
  (** [mount], raising [Invalid_argument] as {!Protocol.resolve_exn} does. *)

  val protocol : ('s, 'svc) store -> ('s, 'svc) protocol_t
  (** The Protocol this Store runs. A Store that replaces it (after a fault, say) opens on
      the same one, so the mounts resolved so far stay valid:
      [Store.open_protocol (Store.protocol store) ~state ~services]. *)

  val create :
    ?clock:Clock.t ->
    ('s, 'svc) protocol_t ->
    state:'s ->
    services:'svc ->
    inbox ->
    (('s, 'svc) store, dispatch_error) result
  (** A Store for [proto], starting from [state] with [services], draining [inbox]. The
      clock defaults to the system's. The Store takes the inbox, even when it refuses it
      as [Foreign_bus] (and closes its bus then); giving it to another Store raises
      [Invalid_argument]. *)

  val create_exn :
    ?clock:Clock.t ->
    ('s, 'svc) protocol_t ->
    state:'s ->
    services:'svc ->
    inbox ->
    ('s, 'svc) store

  val state : ('s, _) store -> 's
  val services : (_, 'svc) store -> 'svc

  val readiness : (_, _) store -> Readiness.t
  (** Readiness for this Store's bus, including closure on a fault. *)

  val add_observer : (_, _) store -> Observe.t -> unit
  val clear_observers : (_, _) store -> unit

  val executions : (_, _) store -> int
  (** Executions run so far: every Action, Effect and Event. *)

  val fault : (_, _) store -> string option
  (** The fault's message, once the Store has faulted; it then refuses everything. Its bus
      closes as it faults, as {!close} closes it: Events still queued are discarded, and
      every later send fails with [Closed]. *)

  val fault_location : (_, _) store -> string option
  (** Where the exception that faulted it was raised, when backtraces are recorded
      ([Printexc.record_backtrace true]). *)

  val session : (_, _) store -> session_id

  val close : (_, _) store -> unit
  (** Closes the bus: Events still queued or held are discarded, and every later send
      fails with [Closed], from any thread. *)

  val add_invariant :
    ?pos:string * int * int * int ->
    ('s, _) store ->
    string ->
    ('s -> invariant_step -> (unit, string) result) ->
    unit
  (** Checks [check] now, then after every root input settles. A failure is not a fault:
      it is reported as a bug at phase [Invariant name], with the invariant's place
      ([pos], [__POS__]), and the Store goes on. A checker that raises is reported at
      [Invariant_panic name] and disabled. A test that must stop at a violation watches
      the bug reports, as [Causet_test.Node] does. *)

  val dispatch_to :
    (_, _) store ->
    ('s, 'svc, 'a, 'e, 'o) mount ->
    'a ->
    (dispatch_outcome, dispatch_error) result
  (** Dispatches an Action to [mount] as a root input: reads the clock, checks admission,
      and runs it with everything it causes before returning. An exception anywhere in
      that work faults the Store. *)

  val dispatch_exn :
    (_, _) store -> ('s, 'svc, 'a, 'e, 'o) mount -> 'a -> dispatch_outcome
  (** [dispatch_to], raising on an error. *)

  val drain_events : (_, _) store -> int -> (int, dispatch_error) result
  (** Delivers up to [max] Events, Store lane first; how many it delivered. Each Event is
      a root input. *)

  val sink : (_, _) store -> sink
  (** The sending side of this Store's bus. *)

  val take_invariants : ('s, _) store -> 's invariants
  (** Takes the invariants out, with their memory, for the Store that replaces this one
      after a restart. This Store has none left. *)

  val restore_invariants : ('s, _) store -> 's invariants -> unit
  (** Moves invariants taken from a previous Store into this one and runs each on the
      restored State ([Restored]), before any root input. Those disabled there stay
      disabled. Give a set of them to one Store only: they keep their memory, which two
      Stores would share. *)
end

(** {1 Reducers alone} *)

type child_call = { slot : string; call_kind : string; call : Message.payload }
(** A call to a child, recorded rather than run: the child is not mounted under a Harness.
*)

(** What a reducer emitted, in order. *)
type ('a, 'e, 'o) emission =
  | Emit_action of 'a
  | Emit_effect of 'e
  | Emit_output of 'o
  | Inline_child of child_call
  | To_child of child_call

(** A component's reducer run alone, on a State given to it, with its emissions returned:
    no Store, no children, no Services. *)
module Harness : sig
  type t

  val create : ?session:session_id -> unit -> t

  val at : t -> timestamp -> unit
  (** Sets the time the next reduction runs at. *)

  val next_time : t -> timestamp
  (** The time the next reduction would run at: [at]'s, or just after the last one when
      the clock stood still or stepped back. *)

  val is_enabled : t -> ('s, _, 'a, _, _) component -> 's -> 'a -> bool

  val reduce :
    t -> ('s, _, 'a, 'e, 'o) component -> 's -> 'a -> 's * ('a, 'e, 'o) emission list
  (** Runs the reducer: the new State and what it emitted. *)

  val dispatch :
    t ->
    ('s, _, 'a, 'e, 'o) component ->
    's ->
    'a ->
    ('s * ('a, 'e, 'o) emission list, [> `Disabled ]) result
  (** [reduce] after the enabling check. *)

  val bug_reports : t -> Observe.Bug_report.t list
  (** The bug reports its reductions made. *)
end
