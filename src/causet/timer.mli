(** The timer Service. A handler asks for a [fired] Event after a delay; it arrives
    through the bus like any reply. A threaded timer keeps its own monotonic clock. A
    host-driven timer uses an injected clock and fires when told; a simulated timer uses
    that mechanism with a manual clock. *)

type fired = { request : Ids.Request_id.t }
(** A timer's deadline passed. [request] is whatever the reducer pinned it to; a reply for
    a request no longer pending is stale and ignored, which is how a timeout is cancelled.
*)

val fired : fired Message.t
(** Its Event type, [causet.timer_fired]. *)

type t

val create : unit -> t
(** A threaded timer: its own thread waits for deadlines or a socket wakeup when timers
    are armed or it is stopped. Native waits work on Unix and Windows without a
    descriptor-number limit. Use {!driven} when the host supplies its own deadline waits.
*)

(** A simulated or host-driven timer's control. *)
module Control : sig
  type t

  val next_deadline : t -> int option
  (** The earliest deadline set and not yet fired. *)

  val pending : t -> int
  (** How many timers are set and not yet fired. *)

  val fire_due : t -> int
  (** Fires every timer whose deadline the clock has reached, in deadline order, by
      sending into the worker lane. A timer whose lane is full stays set, and so do the
      ones after it. A timer whose Store is gone, its bus closed, is dropped and counted
      as fired. Returns how many fired. *)
end

val simulated : Clock.Manual.m -> t * Control.t
(** A timer on [clock], which the Stores of a simulation share, and its control: timers
    fire only when the control fires them, once the clock has reached their deadline. *)

val driven : Clock.t -> t * Control.t
(** A timer on any clock, with no thread of its own. The host arranges a wakeup for
    [Control.next_deadline] and calls [Control.fire_due] on its driver thread. Recheck the
    deadline after root inputs that may have armed an earlier timer. [simulated] uses this
    mechanism on a manual clock. *)

val after : t -> delay:int -> fired Kernel.reply -> Ids.Request_id.t -> unit
(** Sends [fired { request }] through [reply] once [delay] nanoseconds have passed. Timers
    due at the same instant fire in the order they were set. It never blocks, even while
    the timer thread waits for room on a full worker lane. *)

val stop : t -> unit
(** Stops the thread and closes its wake channel, or drops a driven timer's pending
    timers. Stopping again does nothing; after it, [after] sets nothing. *)

(**/**)

val wake_during_drain : unit -> bool * bool
(** For tests: whether a wake landing while the timer thread takes its wake bytes leaves
    the wake flag agreeing with the socket, and whether the next wake still writes. *)

val stop_during_submit : unit -> bool
(** For tests: whether a stop on another thread can close the timer's socket between
    arming a timer and waking the thread. *)
