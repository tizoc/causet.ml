(** The requests a component waits on, kept in its State: for each, what it needs to
    handle the answer, and an optional deadline.

    The first of the answer and the deadline to arrive takes the request ({!take});
    whatever comes after finds nothing and is stale, which is how a reducer tells a late
    answer, a duplicate, or a timer for a request already answered from the one it waits
    for.

    Deadlines are kept two ways:
    - one timer per request, pinned to its ID: the timer's Event calls {!take} like an
      answer does, and whichever comes second is stale;
    - one timer for the earliest deadline ({!next_deadline}): when it fires, {!expire}
      takes every request whose deadline has come, and the next timer is armed for what is
      left.

    A table is a value, like the rest of State: each change returns a new one. Requests
    are kept in ID order, the order they were made in a Store's run. *)

type 'a t

val empty : 'a t

val add : ?deadline:Ids.timestamp -> Ids.Request_id.t -> 'a -> 'a t -> 'a t
(** Waits on [request], with [value], until its answer is taken, or [deadline] comes. A
    request already waited on is replaced. *)

val take : Ids.Request_id.t -> 'a t -> ('a * 'a t) option
(** An answer, or a timer pinned to [request], arrived: the request's value and the table
    without it. [None] when it is stale: taken already, expired, cancelled, or never
    waited on here. It does not compare the timer's time with the deadline: a threaded
    timer keeps its own clock, and a skew must not drop a timeout. *)

val cancel : Ids.Request_id.t -> 'a t -> 'a t
(** Stops waiting on [request] without an answer; a later answer is stale. *)

val retain : (Ids.Request_id.t -> 'a -> bool) -> 'a t -> 'a t
(** Stops waiting on every request [keep] rejects, such as every request of a piece of
    work that has ended. *)

val clear : 'a t -> 'a t
(** Stops waiting on every request, such as when a new one replaces the one in flight:
    [clear], then {!add} the new request, and the old answer is stale. *)

val expire : Ids.timestamp -> 'a t -> (Ids.Request_id.t * 'a) list * 'a t
(** Takes every request whose deadline has come by [now], in deadline order, then request
    order. *)

val next_deadline : 'a t -> Ids.timestamp option
(** The earliest deadline of the requests waited on, for one timer armed at a time. *)

val deadline : Ids.Request_id.t -> 'a t -> Ids.timestamp option
(** The deadline of [request], if it is waited on and has one. *)

val mem : Ids.Request_id.t -> 'a t -> bool
val find_opt : Ids.Request_id.t -> 'a t -> 'a option

val update : Ids.Request_id.t -> ('a -> 'a) -> 'a t -> 'a t
(** [request]'s value changed by [f], if it is waited on. *)

val cardinal : 'a t -> int
val is_empty : 'a t -> bool

val bindings : 'a t -> (Ids.Request_id.t * 'a) list
(** The requests waited on and their values, in request order. *)

val values : 'a t -> 'a list
(** Their values, in request order. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** In Rust's [Debug] form, as the message deriver prints a field of this type. *)
