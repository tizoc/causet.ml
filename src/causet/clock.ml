(* The execution clock. It belongs to the Store and is the only source of
   [meta.time]; Services keep their own clocks for their I/O. Readings are
   nanoseconds and need not increase: the Store makes each execution time
   [max previous now]. *)

type t = { now : unit -> Ids.timestamp }

let now c = c.now ()

(* A clock that moves only when told. Copies share one reading, so a test
   keeps one and moves the Store's clock from outside. *)
module Manual = struct
  type m = int Atomic.t

  let create now : m = Atomic.make now
  let set (m : m) now = Atomic.set m now
  let advance (m : m) by = ignore (Atomic.fetch_and_add m by)
  let get (m : m) = Atomic.get m
  let clock (m : m) = { now = (fun () -> Atomic.get m) }
end

(* A correction to a system clock, in nanoseconds: positive when the
   machine's clock is behind the true time. *)
module Offset = struct
  type o = int Atomic.t

  let create nanos : o = Atomic.make nanos
  let set (o : o) nanos = Atomic.set o nanos
  let get (o : o) = Atomic.get o
end

(* How much monotonic time passes between two readings of the wall clock. *)
let wall_check_nanos = 1_000_000

(* The least the wall clock must gain on the monotonic clock between two
   readings to count as a sleep: far more than sampling error, far less than
   a device's sleep. *)
let min_sleep_nanos = 100_000_000

(* Nanoseconds since the Unix epoch, as near the real time as the machine
   knows it: the wall clock at construction, plus the monotonic time elapsed
   since, plus every stretch the monotonic clock missed (a sleep or suspend,
   when the wall clock gained more). A wall clock stepped back is ignored,
   and readings never go back. [wall] and [monotonic] are the clocks read;
   tests give their own. *)
let of_readings ?offset ~wall ~monotonic () =
  let before = monotonic () in
  let start_wall = wall () in
  let start_monotonic = monotonic () in
  let last_wall = ref start_wall and last_monotonic = ref before and missed = ref 0 in
  let sat_sub a b = if a > b then a - b else 0 in
  let now () =
    let m = monotonic () in
    let m =
      if sat_sub m !last_monotonic >= wall_check_nanos then (
        let w = wall () in
        let after = monotonic () in
        let wall_gained = sat_sub w !last_wall in
        let monotonic_gained = sat_sub after !last_monotonic in
        let ahead = sat_sub wall_gained monotonic_gained in
        if ahead >= min_sleep_nanos then missed := !missed + ahead;
        last_wall := w;
        last_monotonic := m;
        after)
      else m
    in
    let real = start_wall + sat_sub m start_monotonic + !missed in
    match offset with Some o -> real + Offset.get o | None -> real
  in
  { now }

let wall_nanos () = Int64.to_int (Int64.of_float (Unix.gettimeofday () *. 1e9))

(* Seconds on the monotonic clock, since the program started: for bounded
   waits, which a wall clock stepped back or forward must not stretch, and
   small enough to keep a float's full precision. *)
let monotonic_seconds () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9
let monotonic_nanos () = Int64.to_int (Mtime.Span.to_uint64_ns (Mtime_clock.elapsed ()))

(* The machine's clocks. *)
let system ?offset () = of_readings ?offset ~wall:wall_nanos ~monotonic:monotonic_nanos ()
