(* The timer Service. A handler asks for a [fired] Event after a delay; it
   arrives through the bus like any reply. Timers keep their own monotonic
   clock, as Services may: reducers only ever see the Event. A simulated
   timer keeps the same API on a manual clock, and fires when told. *)

open Kernel

(* A timer's deadline passed. [request] is whatever the reducer pinned it
   to; a reply for a request no longer pending is stale and ignored, which
   is how a timeout is cancelled. *)
type fired = { request : Ids.Request_id.t }

let fired : fired Message.t =
  Message.make ~name:"causet.timer_fired"
    ~kind:(fun _ -> "causet.timer_fired")
    ~level:(fun _ -> Message.Trace)
    ~pp:(fun f e -> Format.fprintf f "Fired %a" Ids.Request_id.pp e.request)
    ()

type pending = { reply : fired reply; request : Ids.Request_id.t }

(* Timers in deadline order, first set first at equal deadlines: a map
   keyed by deadline and the order set, so setting and firing cost a
   logarithm of how many are pending. *)
module Queue_ = struct
  module Key = struct
    type t = int * int

    let compare (a, s) (b, t) = match Int.compare a b with 0 -> Int.compare s t | c -> c
  end

  module M = Map.Make (Key)

  type t = { mutable next : int; mutable timers : pending M.t }

  let create () = { next = 0; timers = M.empty }

  let push q at p =
    let seq = q.next in
    q.next <- seq + 1;
    q.timers <- M.add (at, seq) p q.timers

  let peek q =
    Option.map (fun ((at, seq), p) -> (at, seq, p)) (M.min_binding_opt q.timers)

  let pop q =
    match M.min_binding_opt q.timers with
    | Some (k, _) -> q.timers <- M.remove k q.timers
    | None -> ()

  let put_back q (at, seq, p) = q.timers <- M.add (at, seq) p q.timers
  let clear q = q.timers <- M.empty
  let length q = M.cardinal q.timers
end

type driven = {
  clock : Clock.t;
  queue : Queue_.t;
  lock : Mutex.t;
  mutable stopped : bool;
}

type threaded = {
  commands : (float * pending) Queue.t;
  tlock : Mutex.t;
  wakeup : Wakeup.t;
  woken : bool Atomic.t;  (** A wake byte is in the socket, unread. *)
  mutable stopping : bool;
  mutable thread : Thread.t option;
  mutable closed : bool;  (** The wake channel is closed: the timer has stopped. *)
}

type t = Threaded of threaded | Driven of driven

(* Wakes the timer thread. Wakes coalesce into at most one byte in the
   socket, and the write never blocks: a caller is never held up by a timer
   thread that is itself waiting, say for room on a full worker lane. *)
let wake t = if Atomic.compare_and_set t.woken false true then Wakeup.signal t.wakeup

(* Takes the wake bytes, then clears [woken]. A wake between the two finds
   [woken] still set and writes nothing; the command it announces is taken
   at the loop's start, before the next wait. Cleared before the read, a
   wake between them would write a byte the read then takes, leaving
   [woken] set with the socket empty, and every later wake silent. [between]
   runs between the two, for a test to wake there. *)
let drain_wake ?(between = ignore) t =
  Wakeup.clear t.wakeup;
  between ();
  Atomic.set t.woken false

let now_s () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9

(* The timer thread: waits for the next deadline or a new timer, whichever
   comes first, and fires what is due by sending into the worker lane. *)
let run t =
  let timers = Queue_.create () in
  let rec loop () =
    if not t.stopping then (
      Mutex.protect t.tlock (fun () ->
          Queue.iter
            (fun (at, p) -> Queue_.push timers (int_of_float (at *. 1e9)) p)
            t.commands;
          Queue.clear t.commands);
      let rec fire () =
        match Queue_.peek timers with
        | Some ((at, _, p) as timer) when float_of_int at /. 1e9 <= now_s () ->
            Queue_.pop timers;
            ignore timer;
            (* Waits for room on a full worker lane, which holds back the
               timers due after it, until the timer stops; 10 ms at a time,
               as the Rust port does, each wait that finds none counted once
               as a refused send. A closed bus means the Store is gone. *)
            let rec send () =
              if not t.stopping then
                match
                  Reply.send_timeout p.reply { request = p.request } ~timeout:0.01
                with
                | Error (Full _) -> send ()
                | _ -> ()
            in
            send ();
            Mutex.protect t.tlock (fun () ->
                Queue.iter
                  (fun (at, p) -> Queue_.push timers (int_of_float (at *. 1e9)) p)
                  t.commands;
                Queue.clear t.commands);
            fire ()
        | _ -> ()
      in
      fire ();
      (* The native wake channel caps long waits at a day, rechecking a
         deadline further off after each wait. *)
      let timeout =
        match Queue_.peek timers with
        | None -> -1.0
        | Some (at, _, _) -> Float.max 0.0005 ((float_of_int at /. 1e9) -. now_s ())
      in
      if Wakeup.wait t.wakeup ~timeout then drain_wake t;
      loop ())
  in
  loop ()

(* Starts the timer thread. *)
let create () =
  let wakeup = Wakeup.create () in
  let t =
    {
      commands = Queue.create ();
      tlock = Mutex.create ();
      wakeup;
      woken = Atomic.make false;
      stopping = false;
      thread = None;
      closed = false;
    }
  in
  try
    t.thread <- Some (Thread.create run t);
    Threaded t
  with exn ->
    Wakeup.close wakeup;
    raise exn

(* A timer on a host-supplied clock: it fires only when its control is
   driven, once that clock has reached the deadline. *)
let driven clock =
  let driver =
    { clock; queue = Queue_.create (); lock = Mutex.create (); stopped = false }
  in
  (Driven driver, driver)

let simulated clock = driven (Clock.Manual.clock clock)

(* Sends [fired { request }] through [reply] once [delay] nanoseconds have
   passed. Timers due at the same instant fire in the order they were set.
   It never blocks; after [stop] it sets nothing. *)
(* Hands the timer thread a command and wakes it, in one critical section
   with the check that the timer still runs: [stop] marks it stopping under
   the same lock before it closes the socket, so the wake's write never finds
   the socket closed. The write never blocks, so the lock is held only for
   it. [between] runs between the two, for a test to stop the timer
   there. *)
let submit ?(between = ignore) th push =
  Mutex.protect th.tlock (fun () ->
      if not (th.closed || th.stopping) then (
        push ();
        between ();
        wake th))

let after t ~delay (reply : fired reply) request =
  match t with
  | Threaded th ->
      let at = now_s () +. (float_of_int delay /. 1e9) in
      submit th (fun () -> Queue.push (at, { reply; request }) th.commands)
  | Driven sim ->
      Mutex.protect sim.lock (fun () ->
          if not sim.stopped then
            Queue_.push sim.queue (Clock.now sim.clock + delay) { reply; request })

(* Stops the thread and closes its wake channel, or drops a driven timer's
   pending timers. Stopping again does nothing. *)
let stop = function
  | Threaded th ->
      let first =
        Mutex.protect th.tlock (fun () ->
            let first = not th.stopping in
            th.stopping <- true;
            first)
      in
      if first then (
        Atomic.set th.woken false;
        wake th;
        Option.iter Thread.join th.thread;
        th.thread <- None;
        Mutex.protect th.tlock (fun () -> th.closed <- true);
        Wakeup.close th.wakeup)
  | Driven sim ->
      Mutex.protect sim.lock (fun () ->
          sim.stopped <- true;
          Queue_.clear sim.queue)

(* A simulated or host-driven timer's control. *)
module Control = struct
  type t = driven

  (* The earliest deadline set and not yet fired. *)
  let next_deadline sim =
    Mutex.protect sim.lock (fun () ->
        Option.map (fun (at, _, _) -> at) (Queue_.peek sim.queue))

  (* How many timers are set and not yet fired. *)
  let pending sim = Mutex.protect sim.lock (fun () -> Queue_.length sim.queue)

  (* Fires every timer whose deadline the clock has reached, in deadline
     order, by sending into the worker lane. A timer whose lane is full stays
     set, and so do the ones after it. One whose bus is closed is dropped and
     counted as fired, as the Rust port does. Returns how many fired. *)
  let fire_due sim =
    let rec go fired_count =
      let due =
        Mutex.protect sim.lock (fun () ->
            match Queue_.peek sim.queue with
            | Some ((at, _, _) as timer) when at <= Clock.now sim.clock ->
                Queue_.pop sim.queue;
                Some timer
            | _ -> None)
      in
      match due with
      | None -> fired_count
      | Some ((_, _, p) as timer) -> (
          match Reply.try_send_as_worker p.reply { request = p.request } with
          | Error (Full _) ->
              Mutex.protect sim.lock (fun () ->
                  if not sim.stopped then Queue_.put_back sim.queue timer);
              fired_count
          | _ -> go (fired_count + 1))
    in
    go 0
end

(* For tests: a wake that lands between taking the wake bytes and clearing
   [woken]. Returns whether, afterwards, [woken] agrees with the socket (set
   exactly when a byte is waiting), and whether the next wake writes one. *)
let wake_during_drain () =
  let wakeup = Wakeup.create () in
  let t =
    {
      commands = Queue.create ();
      tlock = Mutex.create ();
      wakeup;
      woken = Atomic.make false;
      stopping = false;
      thread = None;
      closed = false;
    }
  in
  let waiting () = Wakeup.wait wakeup ~timeout:0.001 in
  wake t;
  drain_wake t ~between:(fun () -> wake t);
  let agrees = Atomic.get t.woken = waiting () in
  wake t;
  let next_writes = waiting () in
  Wakeup.close wakeup;
  (agrees, next_writes)

(* For tests: a timer stopped from another thread between a command's
   submission and its wake. Returns whether the stop ran to its end, closing
   the socket, before the wake: the stop must wait for the submission. *)
let stop_during_submit () =
  match create () with
  | Driven _ -> false
  | Threaded th as t ->
      let stopper = ref None in
      let stopped = Atomic.make false in
      let stopped_first = ref false in
      let between () =
        stopper :=
          Some
            (Thread.create
               (fun () ->
                 stop t;
                 Atomic.set stopped true)
               ());
        (* Time for the stop to run to the end, which it can only do if the
           submission does not hold the lock. *)
        let deadline = Unix.gettimeofday () +. 0.2 in
        while (not (Atomic.get stopped)) && Unix.gettimeofday () < deadline do
          Thread.yield ()
        done;
        stopped_first := Atomic.get stopped
      in
      (try submit ~between th ignore with _ -> ());
      Option.iter Thread.join !stopper;
      !stopped_first
