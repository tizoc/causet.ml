(* A node mounting the reusable P2P component under its root: its policy,
   routes, reply tokens and two consumers. The contract is documented in
   p2p_node.mli; this file is the Rust port's examples/p2p-node/src/lib.rs. *)

open Causet
open P2p

type req = { key : int; tag : int } [@@deriving debug]
type resp = { tag : int; key : int; value : int } [@@deriving debug]

let resp_of (Peer_id server) (request : req) : resp =
  { tag = request.tag; key = request.key; value = (request.key * 1_000) + server }

module Payloads = struct
  type request = req
  type response = resp

  let pp_request = pp_req
  let pp_response = pp_resp
end

module Node_p2p = P2p.Make (Payloads)

let served_at_once_below = 10

type policy = { fetch : bool; serve : bool } [@@deriving debug]

type outcome = Answered of resp option | Timed_out | Aborted | Failed | Refused
[@@deriving debug]

type fetch = { peer : peer_id; key : int; outcome : outcome option } [@@deriving debug]
type route = { peer : peer_id; id : Rpc_id.t; request : req } [@@deriving debug]
type reply_token = { peer : peer_id; inbound : Inbound_id.t } [@@deriving debug]

let compare_token (a : reply_token) (b : reply_token) =
  match compare a.peer b.peer with 0 -> Inbound_id.compare a.inbound b.inbound | c -> c

let equal_route (a : route) (b : route) =
  a.peer = b.peer && Rpc_id.equal a.id b.id && a.request = b.request

module Correlation_map = Map.Make (struct
  type t = correlation

  let compare = compare
end)

module Token_set = Set.Make (struct
  type t = reply_token

  let compare = compare_token
end)

module Served_map = Map.Make (struct
  type t = peer_id * int

  let compare = compare
end)

type node_state = {
  id : peer_id;
  p2p : Node_p2p.state;
  policy : policy;
  routes : route Correlation_map.t;
  fetches : fetch Correlation_map.t;
  tokens : Token_set.t;
  served : resp Served_map.t;
  next_correlation : int;
}

let new_state id =
  {
    id;
    p2p = Node_p2p.empty;
    policy = { fetch = true; serve = true };
    routes = Correlation_map.empty;
    fetches = Correlation_map.empty;
    tokens = Token_set.empty;
    served = Served_map.empty;
    next_correlation = 0;
  }

let pp_map pp_key pp_value f map =
  Format.fprintf f "{%a}"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
       (fun f (k, v) -> Format.fprintf f "%a: %a" pp_key k pp_value v))
    (Correlation_map.bindings map)

let invariants state =
  match Node_p2p.invariants state.p2p with
  | Error _ as e -> e
  | Ok () -> (
      let outstanding =
        List.fold_left
          (fun acc (peer, (o : Node_p2p.outbound)) ->
            let route : route = { peer; id = o.id; request = o.request } in
            Correlation_map.add o.correlation route acc)
          Correlation_map.empty (Node_p2p.outbound state.p2p)
      in
      let pp_routes = pp_map pp_correlation pp_route in
      if not (Correlation_map.equal equal_route outstanding state.routes) then
        Error
          (Format.asprintf "routes %a do not match the requests out %a" pp_routes
             state.routes pp_routes outstanding)
      else
        match
          Correlation_map.to_seq state.fetches
          |> Seq.find (fun (correlation, (fetch : fetch)) ->
                 fetch.outcome = None <> Correlation_map.mem correlation state.routes)
        with
        | Some (correlation, fetch) ->
            Error
              (Format.asprintf "%a is %a, its route says otherwise" pp_correlation
                 correlation pp_fetch fetch)
        | None -> (
            match
              Token_set.to_seq state.tokens
              |> Seq.find (fun (t : reply_token) ->
                     not (Node_p2p.holds state.p2p t.peer t.inbound))
            with
            | Some token ->
                Error
                  (Format.asprintf "%a names no inbound request P2P holds" pp_reply_token
                     token)
            | None -> Ok ()))

(* ------------------------------------------------------------------------ *)
(* Services *)

type served = { token : reply_token; response : resp option }
[@@deriving message ~name:"node.served"]

type server = { serve : reply_token -> req -> served reply -> unit }
type node_services = { p2p : Node_p2p.services; server : server }

(* ------------------------------------------------------------------------ *)
(* The root *)

type node_action =
  | Connect of { peer : peer_id }
  | Disconnect of { peer : peer_id }
  | Fetch of { peer : peer_id; key : int }
  | Set_policy of { policy : policy }
  | From_p2p of Node_p2p.output
  | Served of { token : reply_token; response : resp option }
[@@deriving message ~name:"node.action"]

type node_effect = Serve of { token : reply_token; request : req }
[@@deriving message ~name:"node.effect"]

let p2p_slot = Child.make "p2p" Node_p2p.component

(* The node's policy over what enters P2P: it refuses only the Intents it
   forbids, and lets P2P's own transitions through. *)
let policy state action _ =
  match Node_p2p.intent action with
  | Some (Rpc_request _) -> state.policy.fetch
  | Some (Rpc_response_pending _ | Rpc_respond _) -> state.policy.serve
  | Some (Dial _ | Disconnect _) | None -> true

let reduce_p2p ctx intent = Ctx.reduce_child ctx p2p_slot (Node_p2p.of_intent intent)

(* Answers an inbound request through P2P; what it sent is recorded. *)
let respond ctx (token : reply_token) response =
  let intent =
    Node_p2p.Rpc_respond { peer = token.peer; inbound = token.inbound; response }
  in
  match (reduce_p2p ctx intent, response) with
  | Causet.Reduced, Some (sent : resp) ->
      Ctx.update ctx (fun s ->
          { s with served = Served_map.add (token.peer, sent.tag) sent s.served })
  | _ -> ()

let end_ state correlation outcome =
  match Correlation_map.find_opt correlation state.fetches with
  | Some (fetch : fetch) ->
      {
        state with
        fetches =
          Correlation_map.add correlation
            { fetch with outcome = Some outcome }
            state.fetches;
      }
  | None -> state

let peer_field (Peer_id n) = [ ("peer", string_of_int n) ]

(* An outbound request ended: its route, matched on all four identities,
   says which fetch. *)
let ended ctx peer id correlation request outcome =
  let state = Ctx.state ctx in
  match Correlation_map.find_opt correlation state.routes with
  | Some (r : route) when equal_route r { peer; id; request } ->
      let state =
        { state with routes = Correlation_map.remove correlation state.routes }
      in
      Ctx.set_state ctx (end_ state correlation outcome)
  | _ ->
      Bug.condition ~fields:(peer_field peer) __POS__ "an ending without its exact route"

let from_p2p ctx (output : Node_p2p.output) =
  let state = Ctx.state ctx in
  match output with
  | Peer_ready _ -> ()
  | Peer_disconnected { peer } ->
      let tokens =
        Token_set.filter (fun (t : reply_token) -> t.peer <> peer) state.tokens
      in
      (* Its request was aborted first: no route is left for it. *)
      let left =
        Correlation_map.fold
          (fun correlation (route : route) acc ->
            if route.peer = peer then correlation :: acc else acc)
          state.routes []
        |> List.rev
      in
      let state =
        List.fold_left
          (fun state correlation ->
            Bug.condition ~fields:(peer_field peer) __POS__
              "a route outlived its peer's disconnection";
            let state =
              { state with routes = Correlation_map.remove correlation state.routes }
            in
            end_ state correlation Aborted)
          { state with tokens } left
      in
      Ctx.set_state ctx state
  | Rpc_request_started { peer; id; correlation; request } -> (
      match Correlation_map.find_opt correlation state.fetches with
      | Some { outcome = None; _ } ->
          Ctx.set_state ctx
            {
              state with
              routes = Correlation_map.add correlation { peer; id; request } state.routes;
            }
      | _ ->
          Bug.condition ~fields:(peer_field peer) __POS__ "a request started for no fetch"
      )
  | Rpc_response_received { peer; id; correlation; request; response } ->
      ended ctx peer id correlation request (Answered response)
  | Rpc_timed_out { peer; id; correlation; request } ->
      ended ctx peer id correlation request Timed_out
  | Rpc_request_aborted { peer; id; correlation; request } ->
      ended ctx peer id correlation request Aborted
  | Rpc_failed { peer; id; correlation; request } ->
      ended ctx peer id correlation request Failed
  | Rpc_request_received { peer; inbound; request } -> (
      let token : reply_token = { peer; inbound } in
      if request.key < served_at_once_below then
        respond ctx token (Some (resp_of state.id request))
      else
        match reduce_p2p ctx (Rpc_response_pending { peer; inbound }) with
        | Causet.Refused -> ()
        | Causet.Reduced ->
            Ctx.update ctx (fun s -> { s with tokens = Token_set.add token s.tokens });
            Ctx.emit_effect ctx (Serve { token; request }))
  (* Expired, or its connection went: an answer to it is inert. *)
  | Rpc_request_expired { peer; inbound } | Rpc_request_dropped { peer; inbound } ->
      Ctx.set_state ctx
        { state with tokens = Token_set.remove { peer; inbound } state.tokens }

let reduce ctx action _ =
  match action with
  | Connect { peer } -> ignore (reduce_p2p ctx (Dial { peer }))
  | Disconnect { peer } -> ignore (reduce_p2p ctx (Node_p2p.Disconnect { peer }))
  | Set_policy { policy } -> Ctx.update ctx (fun s -> { s with policy })
  | Fetch { peer; key } -> (
      let state = Ctx.state ctx in
      let correlation = Correlation state.next_correlation in
      let fetch : fetch = { peer; key; outcome = None } in
      Ctx.set_state ctx
        {
          state with
          next_correlation = state.next_correlation + 1;
          fetches = Correlation_map.add correlation fetch state.fetches;
        };
      let (Correlation tag) = correlation in
      let request : req = { key; tag } in
      (* Refused by the node's policy or by P2P: nothing ran, and the
         consumer is answered now. *)
      match reduce_p2p ctx (Rpc_request { peer; correlation; request }) with
      | Causet.Refused -> Ctx.update ctx (fun s -> end_ s correlation Refused)
      | Causet.Reduced -> ())
  | From_p2p output -> from_p2p ctx output
  | Served { token; response } ->
      let state = Ctx.state ctx in
      if Token_set.mem token state.tokens then (
        Ctx.set_state ctx { state with tokens = Token_set.remove token state.tokens };
        respond ctx token response)

let handle ctx (Serve { token; request }) _ =
  let reply = Effect_ctx.reply ctx served_message in
  (Effect_ctx.service ctx).server.serve token request reply

let node =
  Component.make ~name:"node" ~actions:node_action_message ~effects:node_effect_message
    ~outputs:Message.never ~reduce ~handle
    ~register:(fun r ->
      Registrar.child r p2p_slot
        ~lens:
          (Lens.field
             ~get:(fun (s : node_state) -> s.p2p)
             ~set:(fun (s : node_state) p2p -> { s with p2p }))
        ~service:(fun (svc : node_services) -> svc.p2p)
        ~guard:policy
        ~on_output:(fun _ output port -> Port.emit_action port (From_p2p output))
        ();
      Registrar.event r served_message (fun (event : served) port ->
          Port.emit_action port
            (Served { token = event.token; response = event.response })))
    ()
