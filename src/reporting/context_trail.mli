(** A bounded ring of recent context entries, for reports to include the last few: the
    oldest is overwritten once it is full. *)

type 'a t

val create : int -> 'a t
val capacity : _ t -> int
val length : _ t -> int
val is_empty : _ t -> bool
val clear : _ t -> unit

val push : 'a t -> 'a -> unit
(** Pushes [item], overwriting the oldest if full; nothing when the capacity is zero. *)

val find : 'a t -> ('a -> bool) -> 'a option
(** The first entry, in slot order, that [pred] accepts. *)

val get_or_push : 'a t -> ('a -> bool) -> (unit -> 'a) -> 'a option
(** The first entry [pred] accepts, or a new one from [make], pushed: [None] only when the
    capacity is zero. Entries are values; to update one in place, make them mutable
    records. *)

val recent : 'a t -> int -> 'a list
(** The most recent [max] entries, oldest first. *)

val to_list : 'a t -> 'a list
(** Every entry, oldest first. *)

val iter : ('a -> unit) -> 'a t -> unit
