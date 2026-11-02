(** Testing and simulation on the real Store.

    - {!Node}: a Store on a held bus, with a chosen session and a shared clock. Every
      Event a Service or a test sends is held until the test delivers it, and the
      application's invariants are checked after every root input.
    - {!Scenario}: drives several nodes on one virtual clock. It delivers held Events (by
      default in the order the bus's lanes would run them), advances the clock to the next
      timer deadline or to the next time the application's {!world} names, and stops at a
      predicate, a time or a step budget.
    - {!Rng}: a seeded generator, for the choices a scenario or a simulated network makes;
      {!Rng.split} gives the world a stream of its own.
    - {!Swarm}: the step kinds a run uses, drawn once per run.
    - {!In_flight}: a simulated world's messages in flight.
    - {!check}: generated runs, drawn from one stream of choices, and a failing run shrunk
      to a small list of choices that {!replay} repeats.

    A scenario repeats from its seed, its fixture and any explicit choices only when every
    Service in it is simulated. The guide's testing page shows each of them at work. *)

open Causet

(** {1 Failures} *)

exception Failed of { key : string; message : string }
(** A run's failure, named: what {!check} keys a failure by, and what shrinking keeps
    failing the same way. Node and group invariants raise it; a test may too, with
    {!fail}. Any other exception is keyed by where it was raised. *)

exception Rejected
(** A run the generator rejects: not counted as a run or a failure. *)

val fail : key:string -> string -> 'a
(** Fails the run with [key] and [message]. *)

(** {1 Choices} *)

(** The choices of a run. Every draw is a bounded integer, "3 out of 0..10", and 0 is
    always the simplest answer. A generator from a seed draws with SplitMix64; one that
    {!check} or {!replay} makes also keeps every choice, or replays a list of them, so a
    run is described completely by its list. Copies share one stream. *)
module Rng : sig
  type t

  val create : int -> t

  val seed : t -> int option
  (** The seed it was made from, unless it replays a list. *)

  val choices : t -> int list option
  (** The choices drawn so far, when it keeps them. *)

  val below : t -> int -> int
  (** A number in [0 .. n - 1]; 0 when [n <= 1]. *)

  val next : t -> int
  (** A number below 2^53 - 1, as one choice: the bound every port draws it below, so a
      seed's stream, and every world {!split} seeds from it, is the same in every port. *)

  val split : t -> t
  (** A generator for the run's world: one choice, a seed, and a generator seeded from it
      that records nothing. *)

  val between : t -> int -> int -> int
  (** A number in [low .. high]. *)

  val chance : t -> int -> int -> bool
  (** True with probability [num / den]; a choice of 0 says no. *)

  val more : t -> bool
  (** Whether a run takes another step: 15 times in 16. *)

  val pick : t -> 'a array -> 'a
  (** One of [items], the first being the simplest. *)

  val reject : t -> 'a
  (** Ends this run as rejected (an invalid fixture, say). *)
end

(** The step kinds a run uses, drawn once per run: each on with chance 1/2, all of them if
    none is. *)
module Swarm : sig
  type 'k t

  val create : Rng.t -> ('k * int) list -> 'k t
  (** From [(kind, weight)] pairs. *)

  val pick : 'k t -> Rng.t -> 'k
  (** One of the kinds on, by weight. *)

  val kinds : 'k t -> 'k list
  (** The kinds this run uses. *)
end

(** {1 Nodes and groups} *)

(** Why a group's checks run. *)
type group_step = Added | Published of string | Settled of string | Restarted of string

(** A named group of nodes whose views shared checks read. All its members publish the
    same type of view. *)
module Group : sig
  type 'v t

  val create : string -> 'v t
  val name : _ t -> string

  val check :
    'v t -> string -> ((string * 'v) list -> group_step -> (unit, string) result) -> unit
  (** A shared invariant: what is wrong across the views, by node name, if anything. It
      runs now, then whenever a member publishes, settles a root input or restarts. Its
      captures are its memory. *)

  val leave : _ t -> string -> unit
  (** [node]'s view goes, without running checks. *)

  val view : (string * 'v) list -> string -> 'v option
  (** The view [node] published, if it has. *)
end

(** A real Store under test. Its functions fail the run, naming the node and the step,
    when the Store faults, an invariant fails or a bug is reported. *)
module Node : sig
  type ('s, 'svc) t

  val of_protocol :
    name:string ->
    ('s, 'svc) Protocol.t ->
    state:'s ->
    services:'svc ->
    session:session_id ->
    Clock.Manual.m ->
    ('s, 'svc) t
  (** A node running [proto], starting from [state] with [services], on [clock]. *)

  val create :
    name:string ->
    ('s, 'svc, _, _, _) Component.t ->
    state:'s ->
    services:'svc ->
    session:session_id ->
    Clock.Manual.m ->
    ('s, 'svc) t
  (** A node whose root component is [root]. *)

  val allow_bugs : ('s, 'svc) t -> ('s, 'svc) t
  (** Bug reports are expected: they no longer fail the node. *)

  val bug_reports : _ t -> Observe.Bug_report.t list
  (** The bug reports the Store made. *)

  val with_timer : ('s, 'svc) t -> Timer.Control.t -> ('s, 'svc) t
  (** The node's simulated timers, which {!Scenario} fires. *)

  val name : _ t -> string
  val state : ('s, _) t -> 's
  val store : ('s, 'svc) t -> ('s, 'svc) Store.t
  val sink : _ t -> Sink.t
  val protocol : ('s, 'svc) t -> ('s, 'svc) Protocol.t
  val session : _ t -> session_id
  val mount : _ t -> ('s, 'svc, 'a, 'e, 'o) Path.t -> ('s, 'svc, 'a, 'e, 'o) mount

  val check_with :
    ?pos:string * int * int * int ->
    ('s, 'svc) t ->
    string ->
    ('s -> invariant_step -> (unit, string) result) ->
    ('s, 'svc) t
  (** An invariant, registered on the Store: checked at once, then after every root input;
      a violation fails the node. [f] sees why it runs. *)

  val check :
    ?pos:string * int * int * int ->
    ('s, 'svc) t ->
    string ->
    ('s -> (unit, string) result) ->
    ('s, 'svc) t
  (** [check_with], for an invariant of State alone. *)

  val publish : ('s, 'svc) t -> 'v Group.t -> ('s -> 'v) -> ('s, 'svc) t
  (** Publishes a view of this node's State into [group]: made now, then after each of the
      node's root inputs, for the group's checks to read. *)

  val dispatch : _ t -> (_, _, 'a, _, _) mount -> 'a -> dispatch_outcome
  (** Dispatches an Action as a root input, then checks the invariants. *)

  val held : _ t -> held_event list
  (** The Events held for this node, in send order. *)

  val inspect : _ t -> int -> 'ev Message.t -> 'ev option
  (** The held Event [id], if it is of type [msg]. *)

  val deliver : _ t -> int -> unit
  (** Delivers the held Event [id]: releases it, then runs every released Event as a root
      input, checking the invariants after each. *)

  val deliver_next : _ t -> held_event option
  (** Delivers the held Event the bus would run next, and returns it. *)

  val deliver_where : _ t -> 'ev Message.t -> ('ev -> bool) -> bool
  (** Delivers the first held Event of [msg]'s type for which [select] is true. Returns
      whether there was one. *)

  val discard : _ t -> int -> bool
  (** Drops the held Event [id], as a lost message. *)

  val fire_timers : _ t -> int
  (** Fires the node's simulated timers that are due on the shared clock. *)

  val next_deadline : _ t -> int option

  val close : _ t -> unit
  (** Closes the Store, and leaves the node's groups. *)

  val restart : ('s, 'svc) t -> state:'s -> services:'svc -> session:session_id -> unit
  (** Rebuilds the Store from [state], [services] and [session]. The node's invariants,
      with their memory, and its groups stay: the invariants run first on the restored
      State ([Restored]), then the groups' checks ([Restarted]). The old Store's bus, its
      held Events and the node's timers go: give the new Services' simulated timer with
      {!with_timer}. With a new session, the new Store tells an answer meant for the old
      one apart. *)
end

(** {1 Scenarios} *)

val next_in_queue_order : held_event list -> int option
(** Which of one bus's held Events (as {!Causet.Held.list} gives them, in send order) its
    lanes would run next had none been held: the drain takes the Store lane before the
    worker lane, and each lane holds the Events sent to its head
    ({!Causet.Reply.send_front}) newest first, then the others oldest first. Delivering
    Events one at a time in this order, each with the lanes empty, runs them as production
    would. [None] when none is held. *)

type driven
(** What a scenario can do with a node, whatever its State and Services. *)

val driven : _ Node.t -> driven

type world = { next_time : unit -> int option; advance : int -> unit }
(** The application's world outside the Stores, such as a simulated network: what it will
    do next, and doing it once the clock gets there. *)

val no_world : world
(** A world with nothing in it. *)

(** What a simulated world has in flight: messages due at a time, delivered in that order,
    and in the order they were sent when due together. On a channel (one direction of a
    link), a message is never due before the one sent before it. *)
module In_flight : sig
  type ('m, 'c) t
  (** Messages of type ['m], on channels of type ['c]. *)

  val create : unit -> ('m, 'c) t

  val send_at : ('m, _) t -> int -> 'm -> unit
  (** [message], due at [at]. *)

  val send_in_order : ('m, 'c) t -> 'c -> int -> 'm -> int
  (** [message] on [channel], due at [at] or just after the channel's last message,
      whichever is later. Returns when it is due. *)

  val to_list : ('m, _) t -> (int * 'm) list
  (** What is in flight, in the order it will be delivered, with when each is due. *)

  val next_time : _ t -> int option
  (** When the next message is due. *)

  val due : ('m, _) t -> int -> 'm list
  (** Every message due at [now] or before, in order, taken out. *)

  val length : _ t -> int
  val is_empty : _ t -> bool
  val pp : (Format.formatter -> 'm -> unit) -> Format.formatter -> ('m, _) t -> unit
end

(** One step of a scenario. *)
type step =
  | Delivered of { node : int; event : held_event }
  | Advanced of { to_ : int; fired : int }  (** The clock moved; [fired] timers fired. *)

(** Why a scenario stopped. *)
type stop =
  | Reached  (** The predicate held. *)
  | Quiet  (** Nothing is held or ahead. *)
  | Budget  (** The step budget ran out. *)
  | Time_limit  (** The next thing to do lies past the time limit. *)

(** Drives several nodes on one virtual clock. *)
module Scenario : sig
  type t

  val create : int -> int -> t
  (** A scenario starting at [start] on its clock, with choices from [seed]. *)

  val with_rng : Rng.t -> int -> t
  (** A scenario drawing from [rng], a generated run's. *)

  val clock : t -> Clock.Manual.m
  (** Its clock, which the nodes and simulated timers share. *)

  val rng : t -> Rng.t
  (** The choices it draws from. *)

  val seed : t -> int option

  val log : t -> string list
  (** What each step did, oldest first. *)

  val note : t -> string -> unit
  (** Adds [line] to the log, for a step of the test's own. *)

  val report : t -> int -> string
  (** The failure report: the seed, the step count and the last [last] steps. *)

  val step_with :
    t ->
    driven list ->
    world ->
    choose:((int * held_event) list -> Rng.t -> int) ->
    step option
  (** [step], with [choose] picking which held Event to deliver among every node's. They
      are given as [(node, event)] pairs: [node] the node's index in [nodes], the pairs
      node by node, each node's in send order. [choose] returns an index into them; one
      past the end picks the last. {!next_in_queue_order}, on one node's events, picks as
      the bus would. *)

  val step : t -> driven list -> world -> step option
  (** One step: deliver a held Event if any node has one (the one the first such node's
      bus would run next); otherwise advance the clock to the next timer deadline or world
      time, let the world act, then fire the timers due. [None] when nothing is held or
      ahead. *)

  val run_until :
    ?time_limit:int ->
    t ->
    driven list ->
    world ->
    steps:int ->
    until:(unit -> bool) ->
    stop
  (** Steps until [until] holds, nothing is left, [steps] steps were taken, or the next
      thing to do lies past [time_limit]. *)

  val pending : t -> driven list -> world -> string
  (** What is left: each node's held Events and next timer, and the world's next time. *)
end

(** {1 Generated runs} *)

type config = {
  runs : int;  (** Runs that must pass. Run [i] draws from seed [seed + i]. *)
  seed : int;
  max_rejects : int option;  (** Rejected runs allowed; [None] is ten times [runs]. *)
  max_steps : int;  (** Root inputs a run may take, counted over every Node. *)
  max_shrinks : int;  (** Runs shrinking may make. *)
  determinism : bool;
      (** Makes every passing run a second time from its own choices, and fails the first
          whose second making rejects, or differs in its executions, refusals or inline
          calls on any node, or in their messages' printed forms: something in it reads
          what its choices do not decide. A difference only in State goes unseen; an
          invariant catches that. A message with no printer prints as [_]. Doubles the
          time passing runs that use a [Node] take. *)
}

val default : config
(** 100 runs from seed 0, at most 10,000 root inputs each. *)

type found = {
  seed : int;  (** The seed of the run that failed first. *)
  key : string;  (** What failed, the same for the original and the shrunk run. *)
  choices : int list;  (** The shrunk run's choices: [replay choices f] repeats it. *)
  original : int;  (** How many choices the original run made. *)
  shrinks : int;  (** How many runs shrinking made. *)
  found_max_steps : int;  (** The step budget it was found under. *)
  detail : string option;
      (** For a run that was not deterministic ([key] "not deterministic"), where its two
          makings first differ. *)
}
(** A failing run, found and shrunk. *)

val search : config -> (Rng.t -> unit) -> found option
(** The first failing run of [config]'s, shrunk; [None] when every run passes. It reads no
    environment and replays nothing: it is how a test shows that a planted bug is found.
*)

val replay : ?config:config -> int list -> (Rng.t -> 'a) -> 'a
(** Runs [f] on the choices [choices]: a failure kept as a regression test. *)

val replay_found : found -> (Rng.t -> 'a) -> 'a
(** [replay] of a found failure, under its step budget. *)

val from_env : config -> config
(** [config] with [CAUSET_RUNS] and [CAUSET_SEED] from the environment. *)

val check : ?config:config -> (Rng.t -> unit) -> unit
(** Runs [f] on [config]'s runs, after {!from_env}. A failing run is shrunk, printed with
    the call that repeats it, and replayed, so its own exception fails the test. *)
