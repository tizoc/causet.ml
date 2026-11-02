(** Logging and execution statistics as observers.

    - {!logger}: one log message per execution on the [causet.exec] source, at the level
      the message chooses, with the fields it selects, captured at start; refused Actions
      and inline calls too.
    - {!Stats}: per-type, per-kind counts and durations on one combined execution
      timeline.
    - {!log_at}: application logs stamped with the scheduler time, for reducers and
      handlers, which never read system time. *)

val src : Logs.src
(** The source every execution is logged on, [causet.exec]. *)

val logger : ?label:string -> unit -> Causet.Observe.t
(** Logs every execution as it starts, with the scheduler time, the optional [label] (a
    node's name, say), the category, type, kind, destination path, metadata and the
    message's selected fields. A refused Action is logged with what refused it and where
    it came from; a child's Action reduced inline with [within], its parent's execution,
    at that execution's time, and [root_guard_skipped=true] when its mount has a root
    guard the call did not run; a bug report as an error. With logging off for the source,
    it costs a level check. *)

val log_at : Logs.level -> time:int -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** Logs at [level] stamped with a scheduler time, never the system clock:
    [log_at Logs.Info ~time:meta.time "stale job %d" id]. *)

type range_stats = { total_calls : int; total_duration : int; max_duration : int }
(** Calls and durations in one duration range, in nanoseconds. *)

(** Durations in nine ranges, from under 1 µs to over 50 ms. *)
module Duration_ranges : sig
  type t = range_stats array

  val bounds : int array
  (** The ranges' upper bounds, in nanoseconds; the last range has none. *)

  val names : string array
  (** The ranges' names, such as [under_1_us]. *)

  val merge : t -> t -> unit
  (** Adds [other]'s counts into [t]. *)

  val calls : t -> int
  val total_duration : t -> int
  val max_duration : t -> int
end

(** Execution counts and durations by category, type and kind. Durations use one combined
    timeline: each execution, whatever its category, is charged from its start until the
    next execution starts. An execution is counted when it is charged, so the latest one
    stays out until another starts. *)
module Stats : sig
  type t

  val create : unit -> t

  val observer : t -> Causet.Observe.t
  (** The observer that feeds [t]. *)

  type snapshot = {
    actions : (string * (string * Duration_ranges.t) list) list;
    effects : (string * (string * Duration_ranges.t) list) list;
    events : (string * (string * Duration_ranges.t) list) list;
  }
  (** The statistics so far: for each category, by type then kind, sorted. *)

  val snapshot : t -> snapshot
end
