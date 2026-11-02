(* Synchronous host notifications and native waits for the Event bus. *)
open Kernel_types

let with_bus bus f = Mutex.protect bus.lock (fun () -> f bus.lanes)

let readiness_current l =
  if l.closed then Some Readiness_status.Closed
  else if
    l.store_front <> []
    || (not (Queue.is_empty l.store))
    || l.worker_front <> []
    || not (Queue.is_empty l.worker)
  then Some Readiness_status.Events
  else None

let snapshot_readiness l = Option.bind l.readiness_target (fun target -> target.wake)

let clear_readiness l =
  let notify = snapshot_readiness l in
  Option.iter (fun target -> target.wake <- None) l.readiness_target;
  l.readiness_target <- None;
  notify

(* Notifications only signal a driver. One failing callback cannot undo input. *)
let notify_readiness = function None -> () | Some wake -> ( try wake () with _ -> ())

module Readiness = struct
  type status = Readiness_status.t = Events | Closed
  type error = Already_subscribed
  type t = bus
  type subscription = { bus : bus; target : readiness_target }

  let current bus = with_bus bus readiness_current

  let subscribe bus wake =
    let target = { wake = None } in
    let registered =
      with_bus bus (fun l ->
          if Option.is_some l.readiness_target then Error Already_subscribed
          else (
            if not l.closed then (
              target.wake <- Some wake;
              l.readiness_target <- Some target);
            Ok (Option.is_some (readiness_current l))))
    in
    match registered with
    | Error _ as error -> error
    | Ok ready ->
        if ready then notify_readiness (Some wake);
        Ok { bus; target }

  let cancel { bus; target } =
    with_bus bus (fun l ->
        (match l.readiness_target with
        | Some current when current == target -> l.readiness_target <- None
        | _ -> ());
        target.wake <- None)

  module Blocking = struct
    (* Only the driver waits and closes. Producer callbacks share [gate]
       with close, so an issued notification never writes to a closed socket. *)
    type signal = {
      socket : Wakeup.t;
      gate : Mutex.t;
      mutable closed : bool;
      mutable signalled : bool;
    }

    type waiter = { source : t; subscription : subscription; signal : signal }

    let close_signal signal =
      Mutex.protect signal.gate (fun () ->
          if not signal.closed then (
            signal.closed <- true;
            Wakeup.close signal.socket))

    let signal signal =
      Mutex.protect signal.gate (fun () ->
          if not (signal.closed || signal.signalled) then (
            Wakeup.signal signal.socket;
            signal.signalled <- true))

    let create source =
      let wake =
        {
          socket = Wakeup.create ();
          gate = Mutex.create ();
          closed = false;
          signalled = false;
        }
      in
      try
        match subscribe source (fun () -> signal wake) with
        | Error _ as error ->
            close_signal wake;
            error
        | Ok subscription -> Ok { source; subscription; signal = wake }
      with exn ->
        close_signal wake;
        raise exn

    let close waiter =
      cancel waiter.subscription;
      close_signal waiter.signal

    let check_open signal =
      Mutex.protect signal.gate (fun () ->
          if signal.closed then invalid_arg "causet: a closed readiness waiter")

    let reset signal =
      Mutex.protect signal.gate (fun () ->
          if signal.closed then invalid_arg "causet: a closed readiness waiter";
          if signal.signalled then (
            Wakeup.clear signal.socket;
            signal.signalled <- false))

    let wait_timeout waiter ~timeout =
      if (not (Float.is_finite timeout)) || timeout < 0. then
        invalid_arg "causet: readiness timeout must be finite and nonnegative";
      let deadline = Clock.monotonic_seconds () +. timeout in
      let rec wait () =
        check_open waiter.signal;
        match current waiter.source with
        | Some _ as ready -> ready
        | None -> (
            reset waiter.signal;
            (* A send may have preceded the reset. Rechecking before the wait
               makes either its queued input or its wake byte visible. *)
            match current waiter.source with
            | Some _ as ready -> ready
            | None ->
                let left = deadline -. Clock.monotonic_seconds () in
                if left <= 0. then None
                else (
                  ignore (Wakeup.wait waiter.signal.socket ~timeout:left);
                  wait ()))
      in
      wait ()
  end
end
