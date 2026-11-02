open Ids
open Kernel_types
open Kernel_readiness

(* ------------------------------------------------------------------------ *)
(* The Event bus *)

type 'ev send_error =
  | Full of 'ev  (** The worker lane is at capacity. *)
  | Closed of 'ev  (** The Store is gone. *)
  | Wrong_protocol of 'ev  (** The Mount belongs to another Protocol than this bus. *)
  | Unregistered of 'ev  (** The Mount's component never registered this Event type. *)

(* The sending side, which Services keep. Sends from the Store's own thread
   go to the unbounded Store lane; sends from other threads go to the
   bounded worker lane. A front send goes to the head of the lane it takes;
   each lane runs its front sends, newest first, before the others, oldest
   first. *)
type sink = { ssession : session_id; sproto : protocol; sbus : bus }

(* The receiving side, which the Store takes over. *)
(* The receiving side. One Store takes it, once: [iclaimed] is set as it
   does, as the Rust port's ownership moves it. *)
type inbox = { isink : sink; iclaimed : bool Atomic.t }

module Inbox = struct
  let readiness inbox = inbox.isink.sbus
end

type place = Back | Front | To_worker

(* The Events waiting in the worker lane, its head included: what its
   capacity bounds. *)
let worker_length l = List.length l.worker_front + Queue.length l.worker

let send sink env place deadline =
  let bus = sink.sbus in
  let rec loop () =
    let decided, callbacks =
      with_bus bus (fun l ->
          let done_ result = (`Done result, None) in
          if l.closed then done_ (Error `Closed)
          else
            let was_empty = readiness_current l = None in
            let lane =
              if l.owner = Some (Bug.thread_key ()) && place <> To_worker then Store_lane
              else Worker_lane
            in
            match l.held with
            | Some h ->
                let id = h.next_held in
                h.next_held <- id + 1;
                h.waiting <-
                  h.waiting
                  @ [
                      {
                        h_id = id;
                        h_lane = lane;
                        h_front = place = Front;
                        h_envelope = env;
                      };
                    ];
                done_ (Ok ())
            | None -> (
                if lane = Store_lane then (
                  if place = Front then l.store_front <- env :: l.store_front
                  else Queue.push env l.store;
                  (`Done (Ok ()), if was_empty then snapshot_readiness l else None))
                else if worker_length l < l.worker_capacity then (
                  if place = Front then l.worker_front <- env :: l.worker_front
                  else Queue.push env l.worker;
                  (`Done (Ok ()), if was_empty then snapshot_readiness l else None))
                else
                  match deadline with
                  | Some d when Clock.monotonic_seconds () < d -> (`Wait, None)
                  | _ ->
                      l.rejected_full <- l.rejected_full + 1;
                      done_ (Error `Full)))
    in
    notify_readiness callbacks;
    match decided with
    | `Done r -> r
    | `Wait ->
        Thread.delay 0.0002;
        loop ()
  in
  loop ()

let pop bus =
  with_bus bus (fun l ->
      match l.store_front with
      | e :: rest ->
          l.store_front <- rest;
          Some e
      | [] -> (
          match Queue.take_opt l.store with
          | Some e -> Some e
          | None -> (
              match l.worker_front with
              | e :: rest ->
                  l.worker_front <- rest;
                  Some e
              | [] -> Queue.take_opt l.worker)))

(* Closes the bus: queued and held Events are discarded, later sends fail
   with [Closed] on every thread. *)
let close_bus bus =
  let callbacks =
    with_bus bus (fun l ->
        l.closed <- true;
        l.owner <- None;
        l.store_front <- [];
        Queue.clear l.store;
        l.worker_front <- [];
        Queue.clear l.worker;
        Option.iter (fun h -> h.waiting <- []) l.held;
        clear_readiness l)
  in
  notify_readiness callbacks

(* A typed reply route a Service can keep and send from any thread. *)
type 'ev reply = {
  rsink : sink;
  rmessage : 'ev P.t;
  rinstance : instance_id;
  reffect : execution_id;  (** The Effect this answers, for the trace. *)
}

let envelope sink instance answers (msg : 'ev P.t) (ev : 'ev) =
  {
    e_session = sink.ssession;
    e_instance = instance;
    e_event = Type.Id.uid msg.id;
    e_kind = msg.kind ev;
    e_effect = answers;
    e_payload = P.payload msg ev;
  }

let typed_result ev = function
  | Ok () -> Ok ()
  | Error `Closed -> Error (Closed ev)
  | Error `Full -> Error (Full ev)

module Reply = struct
  type 'ev t = 'ev reply

  let instance r = r.rinstance
  let answers r = r.reffect
  let session r = r.rsink.ssession

  (* Sends the answer without waiting: on the Store's thread to the Store
     lane, which is never full; from any other thread to the worker lane. *)
  let try_send r ev =
    typed_result ev
      (send r.rsink
         (envelope r.rsink r.rinstance (Some r.reffect) r.rmessage ev)
         Back None)

  (* [try_send] as from a worker thread, whatever thread it is on. *)
  let try_send_as_worker r ev =
    typed_result ev
      (send r.rsink
         (envelope r.rsink r.rinstance (Some r.reffect) r.rmessage ev)
         To_worker None)

  (* To the head of its lane: the Store lane on the Store's thread, else the
     worker lane, refused when that is full. *)
  let send_front r ev =
    typed_result ev
      (send r.rsink
         (envelope r.rsink r.rinstance (Some r.reffect) r.rmessage ev)
         Front None)

  (* For worker threads: waits up to [timeout] seconds for room. *)
  let send_timeout r ev ~timeout =
    typed_result ev
      (send r.rsink
         (envelope r.rsink r.rinstance (Some r.reffect) r.rmessage ev)
         Back
         (Some (Clock.monotonic_seconds () +. timeout)))
end

let registered proto index (msg : _ P.t) =
  List.exists (fun b -> b.ev_uid = Type.Id.uid msg.id) proto.mounts.(index).events

module Sink = struct
  type t = sink

  let session s = s.ssession

  let placed sink mount msg ev place deadline =
    if mount.mprotocol <> sink.sproto.pid then Error (Wrong_protocol ev)
    else if not (registered sink.sproto mount.index msg) then Error (Unregistered ev)
    else
      typed_result ev
        (send sink (envelope sink mount.minstance None msg ev) place deadline)

  (* An input that answers no Effect, sent to a mount directly. *)
  let try_send_to sink mount msg ev = placed sink mount msg ev Back None

  (* [try_send_to] as from a worker thread, whatever thread it is on. *)
  let try_send_to_as_worker sink mount msg ev = placed sink mount msg ev To_worker None

  (* [try_send_to], waiting up to [timeout] seconds for room. *)
  let send_timeout_to sink mount msg ev ~timeout =
    placed sink mount msg ev Back (Some (Clock.monotonic_seconds () +. timeout))

  (* A reply route to [mount], as if its Effect [effect] had made it. *)
  let reply_for sink mount msg ~answers =
    if mount.mprotocol <> sink.sproto.pid || not (registered sink.sproto mount.index msg)
    then None
    else
      Some
        { rsink = sink; rmessage = msg; rinstance = mount.minstance; reffect = answers }
end

let next_session = Atomic.make 1

let build_bus proto capacity session held =
  let lanes =
    {
      owner = None;
      store_front = [];
      store = Queue.create ();
      worker_front = [];
      worker = Queue.create ();
      worker_capacity = capacity;
      closed = false;
      rejected_full = 0;
      readiness_target = None;
      held;
    }
  in
  let sink =
    { ssession = session; sproto = proto; sbus = { lock = Mutex.create (); lanes } }
  in
  (sink, { isink = sink; iclaimed = Atomic.make false })

(* ------------------------------------------------------------------------ *)
(* Held Events *)

type held_event = {
  held_id : int;
  held_path : string;
  held_instance : instance_id;
  held_event : string;  (** The Event's type name. *)
  held_kind : string;
  held_answers : execution_id option;
  held_session : session_id;
  held_lane : lane;
  held_front : bool;
}

type release_error = Not_held | Release_full | Release_closed

module Held = struct
  type t = { hbus : bus; hproto : protocol }

  let entries t =
    with_bus t.hbus (fun l -> match l.held with None -> [] | Some h -> h.waiting)

  let list t =
    List.map
      (fun e ->
        let env = e.h_envelope in
        let mount = Hashtbl.find t.hproto.by_instance env.e_instance in
        let def = t.hproto.mounts.(mount) in
        let binding = List.find (fun b -> b.ev_uid = env.e_event) def.events in
        {
          held_id = e.h_id;
          held_path = def.path;
          held_instance = env.e_instance;
          held_event = binding.ev_name;
          held_kind = env.e_kind;
          held_answers = env.e_effect;
          held_session = env.e_session;
          held_lane = e.h_lane;
          held_front = e.h_front;
        })
      (entries t)

  let length t = List.length (entries t)
  let is_empty t = length t = 0

  let inspect t id msg =
    List.find_opt (fun e -> e.h_id = id) (entries t)
    |> Fun.flip Option.bind (fun e -> P.get msg e.h_envelope.e_payload)

  let release t id =
    let result, callbacks =
      with_bus t.hbus (fun l ->
          let refused error = (Error error, None) in
          let was_empty = readiness_current l = None in
          if l.closed then refused Release_closed
          else
            match l.held with
            | None -> refused Not_held
            | Some h -> (
                match List.find_opt (fun e -> e.h_id = id) h.waiting with
                | None -> refused Not_held
                | Some e ->
                    if e.h_lane = Worker_lane && worker_length l >= l.worker_capacity then
                      refused Release_full
                    else (
                      h.waiting <- List.filter (fun x -> x.h_id <> id) h.waiting;
                      (match (e.h_lane, e.h_front) with
                      | Store_lane, true -> l.store_front <- e.h_envelope :: l.store_front
                      | Store_lane, false -> Queue.push e.h_envelope l.store
                      | Worker_lane, true ->
                          l.worker_front <- e.h_envelope :: l.worker_front
                      | Worker_lane, false -> Queue.push e.h_envelope l.worker);
                      (Ok (), if was_empty then snapshot_readiness l else None))))
    in
    notify_readiness callbacks;
    result

  let discard t id =
    with_bus t.hbus (fun l ->
        match l.held with
        | Some h when List.exists (fun e -> e.h_id = id) h.waiting ->
            h.waiting <- List.filter (fun e -> e.h_id <> id) h.waiting;
            true
        | _ -> false)
end
