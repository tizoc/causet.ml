(* A node mounting the reusable [P2p] component under its root, and owning
   what the component must not know: its policy, the routes of its outbound
   requests, the reply tokens of the inbound ones it answers later, and two
   consumers standing in for its subsystems.

   - Policy applies to Intents only: a guard on P2P's binding refuses a fetch
     while fetching is off and an answer while serving is off, and lets every
     internal transition through (timeouts, transport callbacks, cleanup).
     The node sends Intents inline, and a refusal, by its policy or by P2P,
     answers the consumer at once.
   - Routes are keyed by the correlation the node owns, registered from
     [Rpc_request_started] and resolved only when all four identities match.
   - Reply tokens carry an inbound request's exact identity through the
     asynchronous work of answering it.
   - Fetching asks a peer for a key's value; serving answers a peer's fetch,
     at once for small keys, through the [server] Service for the others,
     which may never answer. *)

open Causet
open P2p

(* A fetch: the value of [key], under the requester's [tag]. *)
type req = { key : int; tag : int } [@@deriving debug]

(* A served value, naming the request it answers. *)
type resp = { tag : int; key : int; value : int } [@@deriving debug]

(* What [server] answers to [request]. *)
val resp_of : peer_id -> req -> resp

module Payloads : PAYLOADS with type request = req and type response = resp

(* P2P for the node's requests and responses. *)
module Node_p2p : module type of P2p.Make (Payloads)

(* Keys below this are answered at once; the others through the Server. *)
val served_at_once_below : int

type policy = { fetch : bool; serve : bool }

(* How a fetch ended. *)
type outcome = Answered of resp option | Timed_out | Aborted | Failed | Refused
[@@deriving debug]

(* A fetch offered: what it asked, and how it ended ([None] while P2P has
   its request out). *)
type fetch = { peer : peer_id; key : int; outcome : outcome option }

(* Where an outbound request's ending goes: all four identities must
   match. *)
type route = { peer : peer_id; id : Rpc_id.t; request : req }

(* The exact address of an inbound request being answered. *)
type reply_token = { peer : peer_id; inbound : Inbound_id.t } [@@deriving debug]

module Correlation_map : Map.S with type key = correlation
module Token_set : Set.S with type elt = reply_token

(* What this node sent, keyed by requester and tag. *)
module Served_map : Map.S with type key = peer_id * int

type node_state = private {
  id : peer_id;
  p2p : Node_p2p.state;
  policy : policy;
  routes : route Correlation_map.t;
  fetches : fetch Correlation_map.t;  (** Every fetch offered, by its correlation. *)
  tokens : Token_set.t;
  served : resp Served_map.t;  (** What this node sent, by requester and tag. *)
  next_correlation : int;
}

val new_state : peer_id -> node_state

(* P2P's invariants, and that the node's routes and reply tokens agree with
   what P2P holds. *)
val invariants : node_state -> (unit, string) result

(* ------------------------------------------------------------------------ *)
(* Services *)

(* A served answer, the Event type [node.served]. *)
type served = { token : reply_token; response : resp option } [@@deriving message]

(* Answers a fetch asynchronously, or never. *)
type server = { serve : reply_token -> req -> served reply -> unit }
type node_services = { p2p : Node_p2p.services; server : server }

(* ------------------------------------------------------------------------ *)
(* The root *)

type node_action =
  | Connect of { peer : peer_id }
  | Disconnect of { peer : peer_id }
  | Fetch of { peer : peer_id; key : int }  (** A consumer offers a fetch. *)
  | Set_policy of { policy : policy }
  | From_p2p of Node_p2p.output  (** Lowered from P2P. *)
  | Served of { token : reply_token; response : resp option }
[@@deriving message]

type node_effect = Serve of { token : reply_token; request : req } [@@deriving message]

val node : (node_state, node_services, node_action, node_effect, never) Component.t

(* The slot P2P is mounted at, [/p2p]. *)
val p2p_slot :
  ( node_state * node_services * node_action * node_effect * never,
    Node_p2p.state,
    Node_p2p.services,
    Node_p2p.action,
    Node_p2p.effect_,
    Node_p2p.output )
  Child.t
