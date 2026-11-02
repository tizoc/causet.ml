(* Identities. Times and execution IDs are nanoseconds in a native int: 63
   bits hold nanoseconds since 1970 until the year 2116. *)

type timestamp = int

(* One execution's number in its Store's run: 1, 2, 3, ... in the order
   executions start, so it orders the whole run. Its time is separate
   ([Meta.time]); a new Store counts from 1 again, and its session tells the
   two runs apart. *)
type execution_id = int

(* A mount's identity: xxh3-64 of its path, the same in every run and in both
   ports. *)
type instance_id = int64

(* One Store run's identity. Request IDs and reply routes carry it, so an
   answer meant for another run is told apart. *)
type session_id = int

let pp_instance f id = Format.fprintf f "%016Lx" id

(** @canonical Causet.Request_id *)
module Request_id = struct
  (* A request identity made by a reducer: its run, its execution, and its
     number within that execution, shared with children called inline. *)
  type t = { session : session_id; execution : execution_id; index : int }

  let compare a b =
    match Int.compare a.session b.session with
    | 0 -> (
        match Int.compare a.execution b.execution with
        | 0 -> Int.compare a.index b.index
        | c -> c)
    | c -> c

  let equal a b = compare a b = 0
  let pp f r = Format.fprintf f "R(%d,%d,%d)" r.session r.execution r.index
end
