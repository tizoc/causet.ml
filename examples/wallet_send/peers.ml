(* The network layer: which full peers are connected and which ledger root
   each serves, and the requests sent to them. It gives each request the
   network's own ID, routes the answer back by it, times requests out, and
   reports a disconnect. What a request is for travels with it as an opaque
   [correlation]; the transport is a Service, and the link events
   (connected, announced, disconnected) arrive from it as Events. *)

open Causet
open Domain
open Ledger

(* A full peer. *)
type peer_id = Peer_id of int [@@unboxed] [@@deriving debug]

module Peer_map = Map.Make (struct
  type t = peer_id

  let compare (Peer_id a) (Peer_id b) = Int.compare a b
end)

(* What a request is for, as its sender knows it. Peers carries it back with
   every report on the request, and never reads it. *)
type correlation = Correlation of Request_id.t [@@unboxed] [@@deriving debug]

(* How long a request waits for its answer. *)
let request_timeout = 5_000_000_000

type connection = {
  root : root;  (** The ledger root the peer last announced. *)
  since : int;  (** When it connected, in connection order: older first. *)
}

type outstanding = { peer : peer_id; correlation : correlation }

type state = {
  connected : connection Peer_map.t;
  outstanding : outstanding Request_map.t;
      (** The requests sent and not yet answered, by the network's request ID. *)
  next_since : int;
}

let empty =
  { connected = Peer_map.empty; outstanding = Request_map.empty; next_since = 0 }

(* The connected peers serving [root], longest connected first. *)
let serving state root =
  Peer_map.bindings state.connected
  |> List.filter (fun (_, (c : connection)) -> c.root = root)
  |> List.map (fun (peer, (c : connection)) -> (c.since, peer))
  |> List.sort compare |> List.map snd

(* Every outstanding request is to a connected peer. *)
let invariants state =
  match
    List.find_opt
      (fun (_, (o : outstanding)) -> not (Peer_map.mem o.peer state.connected))
      (Request_map.bindings state.outstanding)
  with
  | Some (request_id, o) ->
      Error
        (Format.asprintf "request %a is outstanding to %a, which is not connected"
           Request_id.pp request_id pp_peer_id o.peer)
  | None -> Ok ()

(* Why a request failed. *)
type peer_error = Timeout | Disconnected [@@deriving debug]

(* ------------------------------------------------------------------------ *)
(* The transport *)

(* A link changed: from the transport, unasked. *)
type link_event =
  | Connected of { peer : peer_id; root : root }
  | Announced of { peer : peer_id; root : root }
  | Disconnected of { peer : peer_id }
[@@deriving message ~name:"peers.link"]

(* A peer's answer, through the Reply its request was sent with: the
   connection it came on, and the request it says it answers. *)
type wire_answer = { peer : peer_id; request_id : Request_id.t; answer : peer_answer }
[@@deriving message ~name:"peers.answer"]

(* The network's Services: the transport, and the timer that times requests
   out. *)
type services = {
  send : peer_id -> Request_id.t -> probe_request -> wire_answer reply -> unit;
  timer : Timer.t;
}

(* ------------------------------------------------------------------------ *)
(* The component *)

type action =
  | Connected of { peer : peer_id; root : root }
  | Announced of { peer : peer_id; root : root }
  | Disconnected of { peer : peer_id }
  | Request of { peer : peer_id; correlation : correlation; request : probe_request }
  | Answered of { peer : peer_id; request_id : Request_id.t; answer : peer_answer }
  | Timed_out of { request_id : Request_id.t }
[@@deriving message ~name:"peers.action"]

type effect_ =
  | Send of { peer : peer_id; request_id : Request_id.t; request : probe_request }
  | Arm of { request_id : Request_id.t }
[@@deriving message ~name:"peers.effect"]

type output =
  | Started of {
      correlation : correlation;
      peer : peer_id;
      request_id : Request_id.t;
      request : probe_request;
    }  (** The request is on its way, under [request_id]. *)
  | Answered of {
      correlation : correlation;
      peer : peer_id;
      request_id : Request_id.t;
      answer : peer_answer;
    }
  | Failed of {
      correlation : correlation;
      peer : peer_id;
      request_id : Request_id.t;
      error : peer_error;
    }
  | Refused of { correlation : correlation; peer : peer_id }
      (** The peer cannot take a request: it is not connected. *)
  | Gone of { peer : peer_id }
      (** The peer disconnected; its outstanding requests have failed first. *)
[@@deriving message ~name:"peers.output"]

type ctx = (state, services, action, effect_, output) Ctx.t

let reduce (ctx : ctx) (action : action) _ =
  let state = Ctx.state ctx in
  match action with
  | Connected { peer; root } ->
      Ctx.set_state ctx
        {
          state with
          next_since = state.next_since + 1;
          connected = Peer_map.add peer { root; since = state.next_since } state.connected;
        }
  | Announced { peer; root } -> (
      match Peer_map.find_opt peer state.connected with
      | Some c ->
          Ctx.set_state ctx
            { state with connected = Peer_map.add peer { c with root } state.connected }
      | None -> ())
  | Disconnected { peer } ->
      if Peer_map.mem peer state.connected then (
        let failed =
          Request_map.bindings state.outstanding
          |> List.filter (fun (_, (o : outstanding)) -> o.peer = peer)
        in
        Ctx.set_state ctx
          {
            state with
            connected = Peer_map.remove peer state.connected;
            outstanding =
              List.fold_left
                (fun m (request_id, _) -> Request_map.remove request_id m)
                state.outstanding failed;
          };
        List.iter
          (fun (request_id, (o : outstanding)) ->
            Ctx.emit_output ctx
              (Failed
                 { correlation = o.correlation; peer; request_id; error = Disconnected }))
          failed;
        Ctx.emit_output ctx (Gone { peer }))
  | Request { peer; correlation; request } ->
      if not (Peer_map.mem peer state.connected) then
        Ctx.emit_output ctx (Refused { correlation; peer })
      else
        let request_id = Ctx.new_request ctx in
        Ctx.set_state ctx
          {
            state with
            outstanding =
              Request_map.add request_id { peer; correlation } state.outstanding;
          };
        Ctx.emit_output ctx (Started { correlation; peer; request_id; request });
        Ctx.emit_effect ctx (Send { peer; request_id; request });
        Ctx.emit_effect ctx (Arm { request_id })
  (* An answer, or a timeout, for a request no longer outstanding (answered,
     timed out, its peer gone) is dropped here, and so is an answer from
     another peer than the one asked: what reaches the sender is about its
     current request. *)
  | Answered { peer; request_id; answer } -> (
      match Request_map.find_opt request_id state.outstanding with
      | Some o when o.peer = peer ->
          Ctx.set_state ctx
            { state with outstanding = Request_map.remove request_id state.outstanding };
          Ctx.emit_output ctx
            (Answered { correlation = o.correlation; peer; request_id; answer })
      | _ -> ())
  | Timed_out { request_id } -> (
      match Request_map.find_opt request_id state.outstanding with
      | Some { peer; correlation } ->
          Ctx.set_state ctx
            { state with outstanding = Request_map.remove request_id state.outstanding };
          Ctx.emit_output ctx (Failed { correlation; peer; request_id; error = Timeout })
      | None -> ())

let handle ctx (eff : effect_) _ =
  match eff with
  | Send { peer; request_id; request } ->
      let reply = Effect_ctx.reply ctx wire_answer_message in
      (Effect_ctx.service ctx).send peer request_id request reply
  | Arm { request_id } ->
      let reply = Effect_ctx.reply ctx Timer.fired in
      Timer.after (Effect_ctx.service ctx).timer ~delay:request_timeout reply request_id

let component : (state, services, action, effect_, output) Component.t =
  Component.make ~name:"peers" ~actions:action_message ~effects:effect_message
    ~outputs:output_message ~reduce ~handle
    ~register:(fun r ->
      Registrar.event r link_event_message (fun (event : link_event) port ->
          Port.emit_action port
            (match event with
            | Connected { peer; root } -> Connected { peer; root }
            | Announced { peer; root } -> Announced { peer; root }
            | Disconnected { peer } -> Disconnected { peer }));
      Registrar.event r wire_answer_message (fun (event : wire_answer) port ->
          Port.emit_action port
            (Answered
               { peer = event.peer; request_id = event.request_id; answer = event.answer }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Timed_out { request_id = fired.request })))
    ()
