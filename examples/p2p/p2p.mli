(* A reusable peer-to-peer RPC component on causet, in a library of its own
   and generic over its messages ([Make] takes them): it knows nothing of the
   node that mounts it.

   - Connections. A peer has as many connections as are up, up to
     [max_connections]: the answer to the dial P2P waits for, and those the
     peer opened. P2P never picks one of them to close, so the two ends of a
     connection never have to agree on one. A connection goes when it closes,
     when a send on it fails, or when the parent disconnects the peer; the
     peer is ready while it holds one. Every transport callback carries the
     connection it belongs to, and a dial's answer the [Dial_id.t] P2P gave
     it: a callback of a connection P2P does not hold, or of a dial it no
     longer waits for, changes nothing, and such a dial's connection is
     closed.
   - Outbound requests. At most one per peer, under a [correlation] the
     parent owns, sent on the connection established last and bound to it. A
     request is started when P2P admits it: it allocates the [Rpc_id.t]
     (which the parent cannot construct) and reports [Rpc_request_started]
     before any I/O. Every started request ends exactly once: answered, timed
     out, aborted when its connection goes, or failed with its own send. The
     abort is reported before the peer gone.
   - Inbound requests. At most [max_inbound] pending per peer, over its
     connections, each identified by a fresh [Inbound_id.t], whatever wire ID
     the peer reused, and answered on the connection it came on. One is
     answered at once or after asynchronous work, expires after
     [serving_timeout], and is dropped when its connection goes; a late
     answer to it is inert.

   The parent sends [intent]s only: [action] is abstract, built from an
   Intent by [of_intent] and read through [intent]; P2P's own transitions
   (transport callbacks, timers) are values the parent cannot build. *)

open Causet

(* How long an outbound request waits for its answer, in nanoseconds. *)
val rpc_timeout : int

(* How long an inbound request waits for the node's answer, in nanoseconds. *)
val serving_timeout : int

(* At most this many inbound requests pending per peer. *)
val max_inbound : int

(* At most this many connections per peer: a peer cannot open them without
   bound. *)
val max_connections : int

type peer_id = Peer_id of int [@@unboxed] [@@deriving debug]

(* A connection, as this end's transport numbers it: unique among one peer's
   connections, and never reused. It has no order: each end numbers its own,
   and may number a dial when it starts. *)
type connection_id = Connection_id of int [@@unboxed] [@@deriving debug]

(* A request's ID on the wire, numbered per connection by its sender: a peer
   reuses them on its next connection. *)
type wire_id = Wire_id of int [@@unboxed] [@@deriving debug]

(* The parent's token for one outbound operation, unique among its
   outstanding ones. *)
type correlation = Correlation of int [@@unboxed] [@@deriving debug]

(* An outbound request, as P2P identifies it. Only P2P makes one: the type is
   abstract, and nothing outside this library builds a value of it. *)
module Rpc_id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(* A dial, as P2P identifies it: fresh for every attempt, and echoed by the
   transport's answer, so the answer to a dial P2P gave up (cancelled by a
   disconnect) cannot end a later one. Only P2P makes one. *)
module Dial_id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(* An inbound request, as P2P identifies it: fresh for every request, so an
   answer to one cannot reach another that reused its wire ID. Only P2P makes
   one. *)
module Inbound_id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Peer_map : Map.S with type key = peer_id
module Connection_map : Map.S with type key = connection_id
module Connection_set : Set.S with type elt = connection_id
module Inbound_map : Map.S with type key = Inbound_id.t

(* Which message a failed send was. *)
type sent = Sent_request of wire_id | Sent_response of wire_id [@@deriving debug]

(* What a request or response can be: any value the parent chooses, with
   its debug form. *)
module type PAYLOADS = sig
  type request
  type response

  val pp_request : Format.formatter -> request -> unit
  val pp_response : Format.formatter -> response -> unit
end

(* The P2P component, for requests [P.request] and responses
   [P.response]. *)
module Make (P : PAYLOADS) : sig
  type request = P.request
  type response = P.response

  (* What crosses a connection. *)
  type wire =
    | Request of { wire : wire_id; request : request }
    | Response of { wire : wire_id; response : response option }
  [@@deriving debug]

  (* The message [message] is, as a failed send names it. *)
  val sent_of : wire -> sent

  (* ---------------------------------------------------------------------- *)
  (* The transport *)

  (* The transport's callbacks, each naming the connection it belongs to. A
     connection's [Connected] comes once, before any other callback of it,
     and none comes after its [Closed] or [Send_failed]. Their Event type,
     [p2p.transport], is sent to P2P's mount. *)
  type transport_event =
    | Connected of { peer : peer_id; connection : connection_id; dial : Dial_id.t option }
        (** A connection is up: the answer to [dial], or, without one, a connection the
            peer opened. *)
    | Dial_failed of { peer : peer_id; dial : Dial_id.t }
    | Received of { peer : peer_id; connection : connection_id; message : wire }
    | Closed of { peer : peer_id; connection : connection_id }
    | Send_failed of { peer : peer_id; connection : connection_id; sent : sent }
        (** The message [sent] could not be sent: the connection is unusable. *)
  [@@deriving message]

  (* The transport: what P2P asks of it. Its answers arrive as
     [transport_event]s, sent to P2P's mount. [dial] is answered by
     [Connected] or [Dial_failed], carrying the dial. *)
  type transport = {
    dial : peer_id -> Dial_id.t -> unit;
    send : peer_id -> connection_id -> wire -> unit;
    close : peer_id -> connection_id -> unit;
  }

  (* P2P's Services. *)
  type services = {
    transport : transport;
    timer : Timer.t;  (** Request timeouts and serving expiry. *)
  }

  (* ---------------------------------------------------------------------- *)
  (* State *)

  type outbound = {
    id : Rpc_id.t;
    wire : wire_id;
    correlation : correlation;
    request : request;
    connection : connection_id;  (** The connection it was sent on, and is answered on. *)
  }

  type inbound = {
    wire : wire_id;
    request : request;
    pending : bool;  (** The node is working on its answer. *)
  }

  (* Read-only outside P2P, as all of its State is. *)
  type connection = private {
    established : int;  (** When P2P saw it established, by its own count. *)
    next_wire : int;
    inbound : inbound Inbound_map.t;
  }

  (* A peer: ready while it holds a connection, dialing while it holds none
     and waits for a dial, disconnected otherwise. *)
  type peer = private {
    connections : connection Connection_map.t;
    dial : Dial_id.t option;
        (** The dial P2P waits for an answer to, kept while connections the peer opened
            come and go. *)
    outbound : outbound option;
    told_gone : bool;
        (** The parent was told the peer gone, and nothing since: not ready again, and no
            [Dial], whether it made a dial or kept the one out. *)
  }

  type state = private {
    peers : peer Peer_map.t;
    established : int;
        (** Connections established so far, for the order of [established]. *)
  }

  (* No peer. *)
  val empty : state

  (* Whether the peer holds a connection. *)
  val is_ready : state -> peer_id -> bool

  (* Whether the peer holds no connection and waits for a dial. *)
  val is_dialing : state -> peer_id -> bool

  (* The dial P2P waits for an answer to, if any. *)
  val dialing : state -> peer_id -> Dial_id.t option

  (* The peer's connections. *)
  val connections : state -> peer_id -> Connection_set.t

  (* Whether the peer's connection is one P2P holds. *)
  val holds_connection : state -> peer_id -> connection_id -> bool

  (* The outbound requests out, by peer. *)
  val outbound : state -> (peer_id * outbound) list

  (* The peer's outbound request, if one is out. *)
  val outbound_to : state -> peer_id -> outbound option

  (* Whether one of the peer's connections holds this inbound request. *)
  val holds : state -> peer_id -> Inbound_id.t -> bool

  (* The inbound requests pending, over every peer. *)
  val inbound_count : state -> int

  (* P2P's own invariants: an outbound request's connection is held, at most
     [max_connections] connections and [max_inbound] inbound requests per
     peer, and no two inbound requests of a connection under one wire ID. *)
  val invariants : state -> (unit, string) result

  (* ---------------------------------------------------------------------- *)
  (* Input: Intents from the parent, and P2P's own transitions *)

  (* What the parent asks of P2P. *)
  type intent =
    | Dial of { peer : peer_id }
        (** Connect to the peer, once it holds no connection. A dial still out is kept,
            and its outcome reported; this is how a parent retries when told the peer
            gone. *)
    | Disconnect of { peer : peer_id }
        (** Close the peer's connections and cancel its dial. Followed by a [Dial], it
            forces a fresh dial, but it is no way to retry: the cancelled dial's
            connection is closed when it comes, which the peer takes for a lost
            connection. *)
    | Rpc_request of { peer : peer_id; correlation : correlation; request : request }
    | Rpc_response_pending of { peer : peer_id; inbound : Inbound_id.t }
        (** The node is working on an inbound request's answer. *)
    | Rpc_respond of {
        peer : peer_id;
        inbound : Inbound_id.t;
        response : response option;
      }

  (* P2P's input, the Action type [p2p.action]: an [intent] from the parent,
     or one of P2P's own transitions, which the parent can neither build nor
     name. *)
  type action [@@deriving message]

  (* The action carrying [intent]: the only one a parent can build. *)
  val of_intent : intent -> action

  (* The Intent it carries, if it is one: what a parent's policy reads. *)
  val intent : action -> intent option

  (* The transport callback it carries, if it is one: what an observer
     reads. *)
  val transport : action -> transport_event option

  (* The Effect type, [p2p.effect]. *)
  type effect_ =
    | Dial_effect of { peer : peer_id; dial : Dial_id.t }
    | Send_effect of { peer : peer_id; connection : connection_id; message : wire }
    | Close_effect of { peer : peer_id; connection : connection_id }
    | Arm_effect of { timer : Request_id.t; after : int  (** nanoseconds *) }
  [@@deriving message]

  (* The facts P2P reports to its parent, the Output type [p2p.output].
     Every outbound one carries the request's four identities. *)
  type output =
    | Peer_ready of { peer : peer_id }  (** Its first connection is up. *)
    | Peer_disconnected of { peer : peer_id }
        (** Its last connection went, or its dial failed, or the parent disconnected it:
            reported once, until it is ready again or the parent connects again. A dial
            may still be out ([dialing]); a new [Dial] keeps it, and its outcome is then
            reported. *)
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
      }  (** Its connection went. *)
    | Rpc_failed of {
        peer : peer_id;
        id : Rpc_id.t;
        correlation : correlation;
        request : request;
      }  (** Its own send failed: its connection is gone too. *)
    | Rpc_request_received of {
        peer : peer_id;
        inbound : Inbound_id.t;
        request : request;
      }
    | Rpc_request_expired of { peer : peer_id; inbound : Inbound_id.t }
        (** Not answered within [serving_timeout]: dropped. *)
    | Rpc_request_dropped of { peer : peer_id; inbound : Inbound_id.t }
        (** Its connection went before it was answered. *)
  [@@deriving message]

  (* ---------------------------------------------------------------------- *)
  (* The component *)

  val component : (state, services, action, effect_, output) Component.t
end
