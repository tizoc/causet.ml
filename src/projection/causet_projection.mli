(** Projections of an ordered log of records into derived facts: the receiving side of
    observability, where stored reports are turned into the facts an application queries
    or alerts on.

    - A {!type-projection} folds records, in log order, into outputs, each with a key,
      keeping recent context bounded by a retention window of event time. It follows the
      determinism guide: ordered collections, no I/O, no wall clock; time is the records'
      own.
    - A {!Projector} feeds each record to its projections, and after a restart warms their
      context up from the stored window, dropping what that would output.
    - {!check_restarts} checks the property restarts rely on: after a restart, a
      projection outputs exactly what an uninterrupted pass did.
    - {!check_incremental} checks the property incremental projections rely on: what a
      projection stores for a key, having recomputed only what each record changed, is
      what recomputing it from every record gives.

    Storage, the cursor, checkpoints and delivery are the application's, and so are the
    contracts that keep them sound. A record's outputs are stored before, or atomically
    with, the cursor's advance past it, and processing resumes strictly after the cursor.
    A receiver that stores outputs before advancing the cursor writes some again after a
    crash, which is safe only if its writes are duplicate-safe and restart equivalence
    holds. A restart rebuilds context, never history, so what a warm-up outputs is
    dropped. A rebuild recomputes outputs only where their records are still stored: from
    the log's beginning, or from a checkpoint. *)

type ('r, 'o) projection = {
  name : string;
  retention_ms : int option;
  process : 'r -> 'o list;
}
(** A projection: its name, how far back in event time its context reaches ([None]: it
    keeps none, and is not warmed up), and what it derives from one record, given in
    order. Its State lives in the closure. *)

val projection :
  ?retention_ms:int -> name:string -> ('r -> 'o list) -> ('r, 'o) projection

(** Feeds records to projections, in the order they were added, and warms them up after a
    restart. *)
module Projector : sig
  type ('r, 'o) t

  val create : time:('r -> int) -> ('r, 'o) t
  (** [time] is a record's event time, in milliseconds: when it happened, not when it
      arrived. *)

  val add : ('r, 'o) t -> ('r, 'o) projection -> unit
  val names : _ t -> string list

  val process : ('r, 'o) t -> 'r -> 'o list
  (** Every projection's outputs for [record], in the order they were added. *)

  val max_retention_ms : _ t -> int option
  (** The longest retention among the projections. *)

  val warm_up_from : _ t -> int -> int option
  (** Starts a warm-up after a restart: [last_time_ms] is the latest event time among the
      records up to the cursor. Each projection's window starts at [last_time_ms] minus
      its retention. Returns the earliest of them, from which the application reads its
      stored log to feed {!warm}; [None] when no projection keeps context. *)

  val warm : ('r, _) t -> 'r -> unit
  (** Feeds a stored record during a warm-up, in log order, to the projections whose
      window it falls in, and drops what they derive: a replayed record may lack context
      older than the window, so its outputs can differ from those stored, and must not
      overwrite them. *)

  val warm_up : ('r, _) t -> 'r list -> unit
  (** The whole warm-up over the records up to the cursor, for a log held in memory. *)
end

type 'o difference = {
  restart : int option;
      (** How many records the latest restart before it had processed; [None] when the two
          passes differ without any restart: the projections are not deterministic. *)
  record : int;  (** The index of the record whose outputs differ. *)
  expected : 'o list;
  actual : 'o list;
}
(** Where a restarted pass first differs from the uninterrupted one. *)

val pp_difference :
  (Format.formatter -> 'o -> unit) -> Format.formatter -> 'o difference -> unit

val check_restarts_with :
  ?equal:('o -> 'o -> bool) ->
  (unit -> ('r, 'o) Projector.t) ->
  ('r list -> ('r, 'o) Projector.t) ->
  'r list ->
  int list ->
  (unit, 'o difference) result
(** Restart equivalence, with the application's own restart: [restarted] is given the
    records up to the cursor and returns the projector ready to go on. Runs [records] once
    without a restart and once restarting before each index in [restarts], and returns the
    first record after a restart whose outputs differ. *)

val check_restarts :
  ?equal:('o -> 'o -> bool) ->
  (unit -> ('r, 'o) Projector.t) ->
  'r list ->
  int list ->
  (unit, 'o difference) result
(** Restart equivalence, with a restart set up the default way: a fresh projector warmed
    up from the records before it. *)

type 'o mismatch = {
  m_record : int;  (** The index of the record after which they differ. *)
  key : string;
  stored : 'o;  (** The key's last output: what the application stores. *)
  recomputed : 'o;  (** The key's output recomputed from every record so far. *)
}
(** Where what a projection stores for a key differs from what recomputing it from every
    record gives. *)

val pp_mismatch :
  (Format.formatter -> 'o -> unit) -> Format.formatter -> 'o mismatch -> unit

val check_incremental :
  ?equal:('o -> 'o -> bool) ->
  key:('o -> string) ->
  projection:('r -> 'o list) ->
  reference:('r -> unit) ->
  recompute:(string -> 'o option) ->
  'r list ->
  (unit, 'o mismatch) result
(** Incremental equals from scratch: a projection that recomputes, after each record, only
    the keys it judges the record to change must store for every key what recomputing it
    from every record would give.

    Feeds each record to [projection] and to [reference] (usually the same projection
    keeping everything it sees), keeps each key's last output as the application stores
    it, and after each record asks [recompute] for each stored key's output now;
    [recompute] closes over both, to say whether the projection still maintains the key
    and to recompute it from the reference. [None] skips the key. Returns the first stored
    output that differs. Only keys the projection has output are compared: one that
    outputs nothing passes, so a test checks that the keys expected are output separately.
*)
