(* A native wake channel. The driver alone waits, clears and closes it;
   callers serialize signals against closing. Waiting leaves the byte in
   place, so each caller's coalescing flag stays valid until it clears. *)
type t

val create : unit -> t
val signal : t -> unit
val clear : t -> unit
val close : t -> unit

(* A negative timeout waits indefinitely. Finite waits are capped at a day;
   callers recheck their predicates and deadlines after every return. *)
val wait : t -> timeout:float -> bool
