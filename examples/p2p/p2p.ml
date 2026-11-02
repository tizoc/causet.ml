(* A reusable peer-to-peer RPC component on causet, generic over its
   messages. The contract is documented in p2p.mli; this file is the Rust
   port's examples/p2p/src/lib.rs, transition for transition. *)

open Causet

let second = 1_000_000_000
let rpc_timeout = 5 * second
let serving_timeout = 10 * second
let max_inbound = 5
let max_connections = 4

type peer_id = Peer_id of int [@@unboxed] [@@deriving debug]
type connection_id = Connection_id of int [@@unboxed] [@@deriving debug]
type wire_id = Wire_id of int [@@unboxed] [@@deriving debug]
type correlation = Correlation of int [@@unboxed] [@@deriving debug]

(* A delay in nanoseconds, printed as Rust prints a [Duration]. *)
type duration = int

let pp_duration f n =
  let unit_of (size, name) =
    if n <> 0 && n mod size = 0 then Some (n / size, name) else None
  in
  match List.find_map unit_of [ (second, "s"); (1_000_000, "ms"); (1_000, "µs") ] with
  | Some (count, name) -> Format.fprintf f "%d%s" count name
  | None -> Format.fprintf f "%dns" n

(* The three identities P2P allocates are request IDs of the execution that
   made them. The interface makes each abstract: [of_request] stays in this
   file. *)
module Request_backed (Name : sig
  val name : string
end) =
struct
  type t = Request_id.t

  let of_request (r : Request_id.t) : t = r
  let request (t : t) : Request_id.t = t
  let compare = Request_id.compare
  let equal = Request_id.equal
  let pp f t = Format.fprintf f "%s(%a)" Name.name Request_id.pp t
end

module Rpc_id = Request_backed (struct
  let name = "RpcId"
end)

module Dial_id = Request_backed (struct
  let name = "DialId"
end)

module Inbound_id = Request_backed (struct
  let name = "InboundId"
end)

module Peer_map = Map.Make (struct
  type t = peer_id

  let compare = compare
end)

module Connection_ord = struct
  type t = connection_id

  let compare = compare
end

module Connection_map = Map.Make (Connection_ord)
module Connection_set = Set.Make (Connection_ord)
module Inbound_map = Map.Make (Inbound_id)

module Wire_set = Set.Make (struct
  type t = wire_id

  let compare = compare
end)

type sent =
  | Sent_request of wire_id [@debug_name "Request"]
  | Sent_response of wire_id [@debug_name "Response"]
[@@deriving debug]

module type PAYLOADS = sig
  type request
  type response

  val pp_request : Format.formatter -> request -> unit
  val pp_response : Format.formatter -> response -> unit
end

module Make (P : PAYLOADS) = struct
  type request = P.request [@@deriving debug]
  type response = P.response [@@deriving debug]

  type wire =
    | Request of { wire : wire_id; request : request }
    | Response of { wire : wire_id; response : response option }
  [@@deriving debug]

  let sent_of = function
    | Request { wire; _ } -> Sent_request wire
    | Response { wire; _ } -> Sent_response wire

  (* ---------------------------------------------------------------------- *)
  (* The transport *)

  type transport_event =
    | Connected of { peer : peer_id; connection : connection_id; dial : Dial_id.t option }
    | Dial_failed of { peer : peer_id; dial : Dial_id.t }
    | Received of { peer : peer_id; connection : connection_id; message : wire }
    | Closed of { peer : peer_id; connection : connection_id }
    | Send_failed of { peer : peer_id; connection : connection_id; sent : sent }
  [@@deriving message ~name:"p2p.transport" ~prefix:"p2p"]

  type transport = {
    dial : peer_id -> Dial_id.t -> unit;
    send : peer_id -> connection_id -> wire -> unit;
    close : peer_id -> connection_id -> unit;
  }

  type services = { transport : transport; timer : Timer.t }

  (* ---------------------------------------------------------------------- *)
  (* State *)

  type outbound = {
    id : Rpc_id.t;
    wire : wire_id;
    correlation : correlation;
    request : request;
    connection : connection_id;
  }

  type inbound = { wire : wire_id; request : request; pending : bool }

  type connection = {
    established : int;
    next_wire : int;
    inbound : inbound Inbound_map.t;
  }

  type peer = {
    connections : connection Connection_map.t;
    dial : Dial_id.t option;
    outbound : outbound option;
    told_gone : bool;
  }

  type state = { peers : peer Peer_map.t; established : int }

  let default_peer =
    {
      connections = Connection_map.empty;
      dial = None;
      outbound = None;
      told_gone = false;
    }

  let empty = { peers = Peer_map.empty; established = 0 }

  (* The connection a new request goes on: the one established last. *)
  let newest (p : peer) =
    Connection_map.fold
      (fun id (c : connection) best ->
        match best with
        | Some (_, at) when at > c.established -> best
        | _ -> Some (id, c.established))
      p.connections None
    |> Option.map fst

  (* The connection holding [inbound]. *)
  let holding (p : peer) inbound =
    Connection_map.to_seq p.connections
    |> Seq.find (fun (_, (c : connection)) -> Inbound_map.mem inbound c.inbound)
    |> Option.map fst

  let peer_inbound_count (p : peer) =
    Connection_map.fold
      (fun _ (c : connection) n -> n + Inbound_map.cardinal c.inbound)
      p.connections 0

  let is_ready state peer =
    match Peer_map.find_opt peer state.peers with
    | Some p -> not (Connection_map.is_empty p.connections)
    | None -> false

  let is_dialing state peer =
    match Peer_map.find_opt peer state.peers with
    | Some p -> Connection_map.is_empty p.connections && p.dial <> None
    | None -> false

  let dialing state peer =
    Option.bind (Peer_map.find_opt peer state.peers) (fun p -> p.dial)

  let connections state peer =
    match Peer_map.find_opt peer state.peers with
    | Some p ->
        Connection_map.fold
          (fun id _ s -> Connection_set.add id s)
          p.connections Connection_set.empty
    | None -> Connection_set.empty

  let holds_connection state peer connection =
    match Peer_map.find_opt peer state.peers with
    | Some p -> Connection_map.mem connection p.connections
    | None -> false

  let outbound state =
    Peer_map.fold
      (fun peer p acc -> match p.outbound with Some o -> (peer, o) :: acc | None -> acc)
      state.peers []
    |> List.rev

  let outbound_to state peer =
    Option.bind (Peer_map.find_opt peer state.peers) (fun p -> p.outbound)

  let holds state peer inbound =
    match Peer_map.find_opt peer state.peers with
    | Some p -> holding p inbound <> None
    | None -> false

  let inbound_count state =
    Peer_map.fold (fun _ p n -> n + peer_inbound_count p) state.peers 0

  let invariants state =
    let check (peer, p) =
      let connections = Connection_map.cardinal p.connections in
      let inbound = peer_inbound_count p in
      match p.outbound with
      | Some o when not (Connection_map.mem o.connection p.connections) ->
          Error
            (Format.asprintf "%a's request is out on %a, which it does not hold"
               pp_peer_id peer pp_connection_id o.connection)
      | _ when connections > max_connections ->
          Error (Format.asprintf "%a has %d connections" pp_peer_id peer connections)
      | _ when inbound > max_inbound ->
          Error (Format.asprintf "%a has %d inbound requests" pp_peer_id peer inbound)
      | _ -> (
          let doubled =
            Connection_map.to_seq p.connections
            |> Seq.find (fun (_, (c : connection)) ->
                   let wires =
                     Inbound_map.fold
                       (fun _ (i : inbound) s -> Wire_set.add i.wire s)
                       c.inbound Wire_set.empty
                   in
                   Wire_set.cardinal wires <> Inbound_map.cardinal c.inbound)
          in
          match doubled with
          | Some (id, _) ->
              Error
                (Format.asprintf "%a holds two inbound requests under one wire ID on %a"
                   pp_peer_id peer pp_connection_id id)
          | None -> Ok ())
    in
    Peer_map.to_seq state.peers
    |> Seq.fold_left
         (fun result entry -> Result.bind result (fun () -> check entry))
         (Ok ())

  (* ---------------------------------------------------------------------- *)
  (* Input: Intents from the parent, and P2P's own transitions *)

  type intent =
    | Dial of { peer : peer_id }
    | Disconnect of { peer : peer_id }
    | Rpc_request of { peer : peer_id; correlation : correlation; request : request }
    | Rpc_response_pending of { peer : peer_id; inbound : Inbound_id.t }
    | Rpc_respond of {
        peer : peer_id;
        inbound : Inbound_id.t;
        response : response option;
      }
  [@@deriving message ~name:"p2p.intent" ~prefix:"p2p"]

  (* Abstract in the interface: a parent builds only [Intent]. *)
  type action =
    | Intent of intent [@nested]
    | Transport of transport_event [@nested]
    | Timer_fired of Request_id.t
  [@@deriving message ~name:"p2p.action"]

  let of_intent intent = Intent intent
  let intent = function Intent intent -> Some intent | _ -> None
  let transport = function Transport event -> Some event | _ -> None

  (* Named apart from the Intents; kinds and printed names are the Rust
     port's. *)
  type effect_ =
    | Dial_effect of { peer : peer_id; dial : Dial_id.t } [@kind "p2p.dial"]
        [@debug_name "Dial"]
    | Send_effect of { peer : peer_id; connection : connection_id; message : wire }
        [@kind "p2p.send"] [@debug_name "Send"]
    | Close_effect of { peer : peer_id; connection : connection_id } [@kind "p2p.close"]
        [@debug_name "Close"]
    | Arm_effect of { timer : Request_id.t; after : duration } [@kind "p2p.arm"]
        [@debug_name "Arm"]
  [@@deriving message ~name:"p2p.effect"]

  type output =
    | Peer_ready of { peer : peer_id }
    | Peer_disconnected of { peer : peer_id }
    | Rpc_request_started of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
      }
    | Rpc_response_received of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
        response : response option;
      }
    | Rpc_timed_out of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
      }
    | Rpc_request_aborted of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
      }
    | Rpc_failed of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
      }
    | Rpc_request_received of {
        peer : peer_id;
        inbound : Inbound_id.t;
        request : request;
      }
    | Rpc_request_expired of { peer : peer_id; inbound : Inbound_id.t }
    | Rpc_request_dropped of { peer : peer_id; inbound : Inbound_id.t }
  [@@deriving message ~name:"p2p.output"]

  (* ---------------------------------------------------------------------- *)
  (* The component *)

  (* What a transition emits, in order: State is set first, then these are
     emitted, as the kernel requires. *)
  type emission = Output of output | Effect of effect_

  let push emits e = Queue.push e emits

  (* Tells the parent the peer is gone, once: a peer whose last connection
     went while its dial is still out is gone already when that dial fails or
     is cancelled. *)
  let report_gone emits peer (p : peer) =
    if p.told_gone then p
    else (
      push emits (Output (Peer_disconnected { peer }));
      { p with told_gone = true })

  (* How a connection went. *)
  type ending =
    | Closed_by_p2p  (** Closed by P2P too: the parent disconnected the peer. *)
    | Closed_by_transport  (** It closed. *)
    | Failed of sent  (** A send on it failed, this one. *)

  (* The peer's connection goes: its outbound request is aborted (or failed,
     if the send that failed was its own) and its inbound requests dropped;
     then, if it was the peer's last, the peer is reported gone. *)
  let drop_connection emits peer (p : peer) connection ending =
    match Connection_map.find_opt connection p.connections with
    | None -> p
    | Some gone ->
        let p = { p with connections = Connection_map.remove connection p.connections } in
        let p =
          match p.outbound with
          | Some ended when ended.connection = connection ->
              let failed =
                match ending with
                | Failed (Sent_request wire) -> wire = ended.wire
                | _ -> false
              in
              let { id; correlation; request; _ } = ended in
              push emits
                (Output
                   (if failed then Rpc_failed { peer; id; correlation; request }
                    else Rpc_request_aborted { peer; id; correlation; request }));
              { p with outbound = None }
          | _ -> p
        in
        Inbound_map.iter
          (fun inbound _ -> push emits (Output (Rpc_request_dropped { peer; inbound })))
          gone.inbound;
        if ending <> Closed_by_transport then
          push emits (Effect (Close_effect { peer; connection }));
        if Connection_map.is_empty p.connections then report_gone emits peer p else p

  let is_enabled state action _ =
    let peer_of peer = Peer_map.find_opt peer state.peers in
    match action with
    (* Whenever the peer holds no connection: a dial still out is kept and
       its outcome reported, so a parent told the peer gone always hears
       again. *)
    | Intent (Dial { peer }) -> (
        match peer_of peer with
        | None -> true
        | Some p -> Connection_map.is_empty p.connections)
    | Intent (Disconnect { peer }) -> (
        match peer_of peer with
        | Some p -> (not (Connection_map.is_empty p.connections)) || p.dial <> None
        | None -> false)
    | Intent (Rpc_request { peer; _ }) -> (
        match peer_of peer with
        | Some p -> (not (Connection_map.is_empty p.connections)) && p.outbound = None
        | None -> false)
    | Intent (Rpc_response_pending { peer; inbound }) -> (
        match peer_of peer with
        | None -> false
        | Some p -> (
            match holding p inbound with
            | None -> false
            | Some connection -> (
                let c = Connection_map.find connection p.connections in
                match Inbound_map.find_opt inbound c.inbound with
                | Some i -> not i.pending
                | None -> false)))
    | Intent (Rpc_respond { peer; inbound; _ }) -> holds state peer inbound
    (* A callback of a connection P2P does not hold is stale: it cannot touch
       the connections P2P has. *)
    | Transport
        ( Received { peer; connection; _ }
        | Closed { peer; connection }
        | Send_failed { peer; connection; _ } ) ->
        holds_connection state peer connection
    (* Only the dial P2P waits for can fail. *)
    | Transport (Dial_failed { peer; dial }) -> (
        match dialing state peer with Some d -> Dial_id.equal d dial | None -> false)
    (* Admitted whatever its dial: a cancelled dial's connection is
       closed. *)
    | Transport (Connected _) | Timer_fired _ -> true

  let with_peer state peer p = { state with peers = Peer_map.add peer p state.peers }

  let intent_reduce ctx emits intent =
    let state = Ctx.state ctx in
    match intent with
    | Dial { peer } ->
        (* A dial still out is kept: replacing it would close its connection
           when it comes, which the peer would take for a lost connection and
           retry, replacing its own. The transport answers every dial;
           Disconnect, then Dial, forces a fresh one, which is no way to
           retry. *)
        let kept = dialing state peer in
        let dial =
          match kept with
          | Some dial -> dial
          | None -> Dial_id.of_request (Ctx.new_request ctx)
        in
        let p = Option.value (Peer_map.find_opt peer state.peers) ~default:default_peer in
        (* Its outcome is reported, whether gone was told already or not. *)
        let p = { p with told_gone = false } in
        let p =
          if kept = None then (
            push emits (Effect (Dial_effect { peer; dial }));
            { p with dial = Some dial })
          else p
        in
        Ctx.set_state ctx (with_peer state peer p)
    | Disconnect { peer } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p ->
            let was_dialing = Connection_map.is_empty p.connections && p.dial <> None in
            let p = { p with dial = None } in
            let p =
              Connection_map.fold
                (fun connection _ p ->
                  drop_connection emits peer p connection Closed_by_p2p)
                p.connections p
            in
            let p = if was_dialing then report_gone emits peer p else p in
            Ctx.set_state ctx (with_peer state peer p))
    | Rpc_request { peer; correlation; request } -> (
        let id = Rpc_id.of_request (Ctx.new_request ctx) in
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p -> (
            match newest p with
            | None -> ()
            | Some connection ->
                let sent_on = Connection_map.find connection p.connections in
                let wire = Wire_id sent_on.next_wire in
                let sent_on = { sent_on with next_wire = sent_on.next_wire + 1 } in
                let p =
                  {
                    p with
                    connections = Connection_map.add connection sent_on p.connections;
                    outbound = Some { id; wire; correlation; request; connection };
                  }
                in
                push emits
                  (Output (Rpc_request_started { peer; id; correlation; request }));
                push emits
                  (Effect
                     (Send_effect
                        { peer; connection; message = Request { wire; request } }));
                push emits
                  (Effect (Arm_effect { timer = Rpc_id.request id; after = rpc_timeout }));
                Ctx.set_state ctx (with_peer state peer p)))
    | Rpc_response_pending { peer; inbound } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p -> (
            match holding p inbound with
            | None -> ()
            | Some connection -> (
                let c = Connection_map.find connection p.connections in
                match Inbound_map.find_opt inbound c.inbound with
                | None -> ()
                | Some pending ->
                    let c =
                      {
                        c with
                        inbound =
                          Inbound_map.add inbound
                            { pending with pending = true }
                            c.inbound;
                      }
                    in
                    let p =
                      {
                        p with
                        connections = Connection_map.add connection c p.connections;
                      }
                    in
                    Ctx.set_state ctx (with_peer state peer p))))
    | Rpc_respond { peer; inbound; response } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p -> (
            (* On the connection it came on. *)
            match holding p inbound with
            | None -> ()
            | Some connection -> (
                let c = Connection_map.find connection p.connections in
                match Inbound_map.find_opt inbound c.inbound with
                | None -> ()
                | Some answered ->
                    let c = { c with inbound = Inbound_map.remove inbound c.inbound } in
                    let p =
                      {
                        p with
                        connections = Connection_map.add connection c p.connections;
                      }
                    in
                    let message = Response { wire = answered.wire; response } in
                    push emits (Effect (Send_effect { peer; connection; message }));
                    Ctx.set_state ctx (with_peer state peer p))))

  let transport_reduce ctx emits event =
    let state = Ctx.state ctx in
    match event with
    | Connected { peer; connection; dial } ->
        let established = state.established in
        let p = Option.value (Peer_map.find_opt peer state.peers) ~default:default_peer in
        (* Told again: it is held already. *)
        if Connection_map.mem connection p.connections then
          Ctx.set_state ctx (with_peer state peer p)
        else
          let answers =
            match (dial, p.dial) with
            | Some d, Some waited -> Dial_id.equal d waited
            | _ -> false
          in
          let p = if answers then { p with dial = None } else p in
          (* A cancelled dial's connection, or one too many: closed. *)
          if
            (dial <> None && not answers)
            || Connection_map.cardinal p.connections >= max_connections
          then (
            push emits (Effect (Close_effect { peer; connection }));
            Ctx.set_state ctx (with_peer state peer p))
          else
            let first = Connection_map.is_empty p.connections in
            let added : connection =
              { established; next_wire = 0; inbound = Inbound_map.empty }
            in
            let p =
              { p with connections = Connection_map.add connection added p.connections }
            in
            let p =
              if first then (
                push emits (Output (Peer_ready { peer }));
                { p with told_gone = false })
              else p
            in
            Ctx.set_state ctx
              { peers = Peer_map.add peer p state.peers; established = established + 1 }
    | Dial_failed { peer; _ } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p ->
            let p = { p with dial = None } in
            let p =
              if Connection_map.is_empty p.connections then report_gone emits peer p
              else p
            in
            Ctx.set_state ctx (with_peer state peer p))
    | Received { peer; connection; message = Request { wire; request } } -> (
        let inbound = Inbound_id.of_request (Ctx.new_request ctx) in
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p -> (
            (* Past the peer's capacity, or a wire ID still pending on this
               connection: dropped, as the peer's own request timeout will
               tell it. *)
            let full = peer_inbound_count p >= max_inbound in
            match Connection_map.find_opt connection p.connections with
            | None -> ()
            | Some on ->
                if
                  full
                  || Inbound_map.exists (fun _ (i : inbound) -> i.wire = wire) on.inbound
                then ()
                else
                  let pending = { wire; request; pending = false } in
                  let on =
                    { on with inbound = Inbound_map.add inbound pending on.inbound }
                  in
                  let p =
                    {
                      p with
                      connections = Connection_map.add connection on p.connections;
                    }
                  in
                  push emits (Output (Rpc_request_received { peer; inbound; request }));
                  push emits
                    (Effect
                       (Arm_effect
                          { timer = Inbound_id.request inbound; after = serving_timeout }));
                  Ctx.set_state ctx (with_peer state peer p)))
    | Received { peer; connection; message = Response { wire; response } } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p -> (
            (* Only on the connection the request went on. *)
            match p.outbound with
            | Some answered when answered.connection = connection && answered.wire = wire
              ->
                push emits
                  (Output
                     (Rpc_response_received
                        {
                          peer;
                          id = answered.id;
                          correlation = answered.correlation;
                          request = answered.request;
                          response;
                        }));
                Ctx.set_state ctx (with_peer state peer { p with outbound = None })
            | _ -> ()))
    | Closed { peer; connection } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p ->
            let p = drop_connection emits peer p connection Closed_by_transport in
            Ctx.set_state ctx (with_peer state peer p))
    | Send_failed { peer; connection; sent } -> (
        match Peer_map.find_opt peer state.peers with
        | None -> ()
        | Some p ->
            let p = drop_connection emits peer p connection (Failed sent) in
            Ctx.set_state ctx (with_peer state peer p))

  let timer_reduce ctx emits timer =
    let state = Ctx.state ctx in
    let inbound = Inbound_id.of_request timer in
    let ended (peer, p) =
      match p.outbound with
      | Some timed_out when Request_id.equal (Rpc_id.request timed_out.id) timer ->
          push emits
            (Output
               (Rpc_timed_out
                  {
                    peer;
                    id = timed_out.id;
                    correlation = timed_out.correlation;
                    request = timed_out.request;
                  }));
          Some (peer, { p with outbound = None })
      | _ ->
          Connection_map.to_seq p.connections
          |> Seq.find (fun (_, (c : connection)) -> Inbound_map.mem inbound c.inbound)
          |> Option.map (fun (connection, (c : connection)) ->
                 push emits (Output (Rpc_request_expired { peer; inbound }));
                 let c = { c with inbound = Inbound_map.remove inbound c.inbound } in
                 ( peer,
                   { p with connections = Connection_map.add connection c p.connections }
                 ))
    in
    match Seq.find_map ended (Peer_map.to_seq state.peers) with
    | Some (peer, p) -> Ctx.set_state ctx (with_peer state peer p)
    | None -> ()

  let reduce ctx action _ =
    let emits = Queue.create () in
    (match action with
    | Intent intent -> intent_reduce ctx emits intent
    | Transport event -> transport_reduce ctx emits event
    | Timer_fired timer -> timer_reduce ctx emits timer);
    Queue.iter
      (function
        | Output output -> Ctx.emit_output ctx output
        | Effect eff -> Ctx.emit_effect ctx eff)
      emits

  let handle ctx eff _ =
    match eff with
    | Dial_effect { peer; dial } -> (Effect_ctx.service ctx).transport.dial peer dial
    | Send_effect { peer; connection; message } ->
        (Effect_ctx.service ctx).transport.send peer connection message
    | Close_effect { peer; connection } ->
        (Effect_ctx.service ctx).transport.close peer connection
    | Arm_effect { timer; after } ->
        let reply = Effect_ctx.reply ctx Timer.fired in
        Timer.after (Effect_ctx.service ctx).timer ~delay:after reply timer

  let component =
    Component.make ~name:"p2p" ~actions:action_message ~effects:effect_message
      ~outputs:output_message ~is_enabled ~reduce ~handle
      ~register:(fun r ->
        Registrar.event r transport_event_message (fun event port ->
            Port.emit_action port (Transport event));
        Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
            Port.emit_action port (Timer_fired fired.request)))
      ()
end
