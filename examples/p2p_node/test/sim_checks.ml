(* Cross-node and execution-order invariant checks. *)

open Causet
open P2p
open P2p_node
open Sim_common
open Sim_network

(* ------------------------------------------------------------------------ *)
(* Checks *)

(* What a node shows the group: what it served, and what it was answered
   ([(server, key, tag, answer)]). *)
type view = { sent : resp Served_map.t; answers : (peer_id * int * int * resp) list }

let view (state : node_state) =
  {
    sent = state.served;
    answers =
      Correlation_map.fold
        (fun (Correlation tag) (fetch : fetch) acc ->
          match fetch.outcome with
          | Some (Answered (Some answer)) -> (fetch.peer, fetch.key, tag, answer) :: acc
          | _ -> acc)
        state.fetches []
      |> List.rev;
  }

let name (Peer_id n) = Printf.sprintf "node-%d" n

let group_checks group peers =
  Group.check group "an answer is the one its peer sent for that request" (fun views _ ->
      let wrong me =
        match Group.view views (name me) with
        | None -> None
        | Some mine ->
            List.find_map
              (fun (server, key, tag, (answer : resp)) ->
                let sent =
                  Option.bind
                    (Group.view views (name server))
                    (fun theirs -> Served_map.find_opt (me, tag) theirs.sent)
                in
                if answer.tag <> tag || answer.key <> key || sent <> Some answer then
                  Some
                    (Format.asprintf "%a took %a from %a for key %d tag %d; it sent %a"
                       pp_peer_id me pp_resp answer pp_peer_id server key tag
                       (fun f -> function
                         | None -> Format.pp_print_string f "None"
                         | Some r -> Format.fprintf f "Some(%a)" pp_resp r)
                       sent)
                else None)
              mine.answers
      in
      match List.find_map wrong peers with Some e -> Error e | None -> Ok ())

(* Check 4: in the order the node lowered them, an abort for a peer never
   follows its disconnection without a new request between. Per peer,
   whether its disconnection is the latest thing seen of it; and the
   violations. *)
type order = {
  mutable gone_last : bool Peer_map.t;
  mutable order_violations : string list;
}

let order_observer order =
  {
    Observe.none with
    action_start =
      (fun _ payload ->
        match Message.get node_action_message payload with
        | Some (From_p2p (Peer_disconnected { peer })) ->
            order.gone_last <- Peer_map.add peer true order.gone_last
        | Some (From_p2p (Rpc_request_started { peer; _ })) ->
            order.gone_last <- Peer_map.add peer false order.gone_last
        | Some (From_p2p (Rpc_request_aborted { peer; _ }))
          when Option.value (Peer_map.find_opt peer order.gone_last) ~default:false ->
            order.order_violations <-
              Format.asprintf "%a: aborted after its disconnection" pp_peer_id peer
              :: order.order_violations
        | _ -> ());
  }

let p2p_path = "/p2p"

(* Every input reaching P2P is accounted for, from the observer calls
   alone: an Event at P2P (a transport callback or a timer) is acted on or
   refused, as the Action its adapter queued, before it settles; an Intent
   the node calls inline for a connect or a disconnect is reduced or refused
   within the node's execution. *)
type accounts = {
  mutable pending_callbacks : Int_set.t;
      (** Events at P2P whose Action was neither acted on nor refused yet. *)
  mutable pending_intents : Int_set.t;
      (** The node's executions whose Intent was neither reduced nor refused. *)
  mutable account_violations : string list;
}

let accounts_observer a =
  let violate line = a.account_violations <- line :: a.account_violations in
  {
    Observe.none with
    event_start =
      (fun exec _ ->
        if exec.path = p2p_path then (
          a.pending_callbacks <- Int_set.add exec.meta.id a.pending_callbacks;
          incr callbacks));
    action_start =
      (fun exec payload ->
        if exec.path = p2p_path then
          a.pending_callbacks <-
            Int_set.remove (Meta.causal_execution_id exec.meta) a.pending_callbacks
        else
          match Message.get node_action_message payload with
          | Some (Connect _ | Disconnect _) ->
              a.pending_intents <- Int_set.add exec.meta.id a.pending_intents;
              incr intents
          | _ -> ());
    inline_action =
      (fun inline _ ->
        if inline.path = p2p_path then
          a.pending_intents <- Int_set.remove inline.within a.pending_intents);
    action_refused =
      (fun refusal _ ->
        if refusal.path = p2p_path then
          match refusal.sent with
          | Queued origin ->
              if Int_set.mem origin.causal a.pending_callbacks then (
                a.pending_callbacks <- Int_set.remove origin.causal a.pending_callbacks;
                incr callbacks_refused)
          | From_inline within ->
              if Int_set.mem within a.pending_intents then (
                a.pending_intents <- Int_set.remove within a.pending_intents;
                incr intents_refused)
          | Root -> violate (refusal.kind ^ ": refused, sent Root")
          | From_effect at ->
              violate (Printf.sprintf "%s: refused, sent From_effect %d" refusal.kind at));
    action_finish =
      (fun exec ->
        if Int_set.mem exec.meta.id a.pending_intents then (
          a.pending_intents <- Int_set.remove exec.meta.id a.pending_intents;
          violate (exec.kind ^ ": its Intent was neither reduced nor refused")));
    event_settled =
      (fun exec ->
        if Int_set.mem exec.meta.id a.pending_callbacks then (
          a.pending_callbacks <- Int_set.remove exec.meta.id a.pending_callbacks;
          violate (exec.kind ^ ": neither acted on nor refused")));
  }

(* What a node's P2P is executing, for the effects and outputs it makes. *)
type cause = Disconnect_of of peer_id | Transport_of of N.transport_event | Other

(* Checks 5 to 7, from the node's executions: why P2P closes a connection,
   which dial can fail or make the peer ready, which send a failed
   request's was, which message answered a request, that a connect to a
   peer P2P holds no connection to dials unless a dial is out, and that no
   dial goes out while another is. The dial waited for is the last one asked
   of the transport, until answered or the node disconnects the peer. None
   of this restates how P2P chooses among connections, only what it may do.

   The node's Intents reach P2P inline, and are read as P2P's own, through
   [inline_action]; transport callbacks are P2P's queued ones. A lowered
   output's action runs inside the execution that made it, so it leaves the
   cause. *)
type causes = {
  mutable cause : cause;
  mutable awaited : Dial_id.t Peer_map.t;
  mutable answering : (peer_id * connection_id) option;
      (** The connection of the [Connected] executing, if it answers the dial waited for.
      *)
  mutable last_request : (connection_id * wire_id) Peer_map.t;
      (** Each peer's last request sent: its connection and wire ID. *)
  mutable held : Connection_set.t Peer_map.t;
      (** What P2P holds: each connection from its admitted [Connected] until it closes,
          fails, or P2P closes it; for the limit. *)
  mutable reported : (bool * bool) Peer_map.t;
      (** Each peer's last readiness report (ready or gone), and whether a report is owed
          since: a dial went out, or a connect kept the one out. *)
  mutable connecting : (peer_id * int) option;
      (** P2P was asked to dial a peer it holds no connection to and is not dialing,
          inline within this execution: a dial must go out before it settles. *)
  mutable gone : peer_id list;  (** The peers reported gone, for the parent's retries. *)
  mutable cause_violations : string list;
}

let new_causes () =
  {
    cause = Other;
    awaited = Peer_map.empty;
    answering = None;
    last_request = Peer_map.empty;
    held = Peer_map.empty;
    reported = Peer_map.empty;
    connecting = None;
    gone = [];
    cause_violations = [];
  }

let held_none c peer =
  match Peer_map.find_opt peer c.held with
  | None -> true
  | Some s -> Connection_set.is_empty s

let held_update c peer f =
  let s = Option.value (Peer_map.find_opt peer c.held) ~default:Connection_set.empty in
  c.held <- Peer_map.add peer (f s) c.held

let owe_report c peer =
  let ready, _ =
    Option.value (Peer_map.find_opt peer c.reported) ~default:(false, false)
  in
  c.reported <- Peer_map.add peer (ready, true) c.reported

let awaits c peer dial =
  match Peer_map.find_opt peer c.awaited with
  | Some d -> Dial_id.equal d dial
  | None -> false

let causes_observer c =
  let violate fmt =
    Format.kasprintf (fun line -> c.cause_violations <- line :: c.cause_violations) fmt
  in
  {
    Observe.none with
    action_start =
      (fun _ payload ->
        match Message.get node_action_message payload with
        | Some action -> (
            match action with
            | From_p2p (Rpc_failed { peer; _ }) ->
                let own =
                  match c.cause with
                  | Transport_of
                      (Send_failed { peer = failed; connection; sent = Sent_request wire })
                    ->
                      failed = peer
                      && Peer_map.find_opt peer c.last_request = Some (connection, wire)
                  | _ -> false
                in
                if not own then
                  violate "%a: failed, not after its request's send" pp_peer_id peer
            (* Answered only on its request's connection, with its wire
               ID. *)
            | From_p2p (Rpc_response_received { peer; _ }) ->
                let own =
                  match c.cause with
                  | Transport_of
                      (Received
                         { peer = from; connection; message = Response { wire; _ } }) ->
                      from = peer
                      && Peer_map.find_opt peer c.last_request = Some (connection, wire)
                  | _ -> false
                in
                if not own then
                  violate "%a: answered off its request's connection" pp_peer_id peer
            (* Gone once, but again after a connect. *)
            | From_p2p (Peer_disconnected { peer }) ->
                if Peer_map.find_opt peer c.reported = Some (false, false) then
                  violate "%a: reported gone twice" pp_peer_id peer;
                c.reported <- Peer_map.add peer (false, false) c.reported;
                c.gone <- c.gone @ [ peer ]
            (* Ready only by the dial waited for, or a connection the peer
               opened; and not twice. *)
            | From_p2p (Peer_ready { peer }) ->
                (match Peer_map.find_opt peer c.reported with
                | Some (true, _) -> violate "%a: reported ready twice" pp_peer_id peer
                | _ -> ());
                c.reported <- Peer_map.add peer (true, false) c.reported;
                let by =
                  match c.cause with
                  | Transport_of (Connected { peer = arriving; connection; dial }) ->
                      arriving = peer
                      && (dial = None || c.answering = Some (peer, connection))
                  | _ -> false
                in
                if not by then
                  violate "%a: ready, not by a dial it waits for" pp_peer_id peer
            | From_p2p _ -> ()
            | _ -> c.cause <- Other)
        | None -> (
            match Message.get N.action_message payload with
            | None -> ()
            | Some action -> (
                c.answering <- None;
                match N.transport action with
                | None -> c.cause <- Other
                | Some event ->
                    (match event with
                    | Dial_failed { peer; dial } ->
                        if not (awaits c peer dial) then
                          violate "%a: a dial it no longer waits for failed it" pp_peer_id
                            peer;
                        c.awaited <- Peer_map.remove peer c.awaited
                    | Connected { peer; connection; dial } ->
                        (match dial with
                        | Some dial when awaits c peer dial ->
                            c.awaited <- Peer_map.remove peer c.awaited;
                            c.answering <- Some (peer, connection)
                        | _ -> ());
                        held_update c peer (Connection_set.add connection)
                    | Closed { peer; connection } | Send_failed { peer; connection; _ } ->
                        held_update c peer (Connection_set.remove connection)
                    | Received _ -> ());
                    c.cause <- Transport_of event)));
    (* P2P's own Intents, which the node calls inline: read as P2P's, so any
       parent calling it inline is observed alike. *)
    inline_action =
      (fun inline payload ->
        match Option.bind (Message.get N.action_message payload) N.intent with
        | Some (Disconnect { peer }) ->
            c.awaited <- Peer_map.remove peer c.awaited;
            c.held <- Peer_map.remove peer c.held;
            c.cause <- Disconnect_of peer
        (* Holding none, it dials; already dialing, that dial is kept and owes
           a report again. *)
        | Some (Dial { peer }) ->
            if held_none c peer then
              if Peer_map.mem peer c.awaited then owe_report c peer
              else c.connecting <- Some (peer, inline.within);
            c.cause <- Other
        | _ -> ());
    (* A connect is refused only while P2P holds a connection to the peer. *)
    action_refused =
      (fun _ payload ->
        match Option.bind (Message.get N.action_message payload) N.intent with
        | Some (Dial { peer }) ->
            if held_none c peer then
              violate "%a: a connect refused, holding none" pp_peer_id peer
        | _ -> ());
    action_settled =
      (fun exec ->
        match c.connecting with
        | Some (peer, within) when within = exec.meta.id ->
            c.connecting <- None;
            violate "%a: asked to connect, holding none, and no dial went out" pp_peer_id
              peer
        | _ -> ());
    effect_start =
      (fun _ payload ->
        match Message.get N.effect_message payload with
        | Some (Dial_effect { peer; dial }) -> (
            (* A dial replacing one still out would close the other's
               connection when it came. *)
            if Peer_map.mem peer c.awaited then
              violate "%a: dialed while a dial was still out" pp_peer_id peer;
            c.awaited <- Peer_map.add peer dial c.awaited;
            owe_report c peer;
            match c.connecting with
            | Some (connecting, _) when connecting = peer -> c.connecting <- None
            | _ -> ())
        | Some (Send_effect { peer; connection; message = Request { wire; _ } }) ->
            c.last_request <- Peer_map.add peer (connection, wire) c.last_request
        | Some (Close_effect { peer; connection }) ->
            let allowed =
              match c.cause with
              | Disconnect_of disconnected -> disconnected = peer
              | Transport_of (Send_failed { peer = failed; connection = on; _ }) ->
                  failed = peer && on = connection
              (* The arriving connection only: a cancelled dial's, or any past
                 the limit (the dial's answer, or one the peer opened, is kept
                 below it). *)
              | Transport_of (Connected { peer = arriving; connection = on; dial }) ->
                  let others =
                    match Peer_map.find_opt peer c.held with
                    | Some held -> Connection_set.cardinal held - 1
                    | None -> 0
                  in
                  let cancelled =
                    dial <> None && c.answering <> Some (peer, connection)
                  in
                  arriving = peer && on = connection
                  && (cancelled || others >= max_connections)
              | _ -> false
            in
            if not allowed then
              violate "%a: closed %a for no reason" pp_peer_id peer pp_connection_id
                connection;
            held_update c peer (Connection_set.remove connection)
        | _ -> ());
  }

let p2p_of (node : (node_state, node_services) Harness.t) : N.state =
  (Harness.state node).p2p

(* Check 8, once quiet: both ends hold every link that is up, each by its
   own number, and no node holds a connection whose link is down, or is
   still dialing; each node's last readiness report of a peer is whether it
   is ready; and no dial is left whose ending it was never told. *)
let links_agree net all ids causes =
  let index node =
    match List.find_index (fun id -> id = node) ids with
    | Some i -> i
    | None -> failwith "a node"
  in
  Int_map.iter
    (fun _ link ->
      if link.up then
        let a, b = link.ends in
        List.iter
          (fun (node, number_) ->
            let peer, _ = other_end link node in
            if not (N.holds_connection (p2p_of all.(index node)) peer number_) then
              failwith
                (Format.asprintf "%a does not hold %a with %a, which is up" pp_peer_id
                   node pp_connection_id number_ pp_peer_id peer))
          [ a; b ])
    net.links;
  List.iteri
    (fun i me ->
      let p2p = p2p_of all.(i) in
      List.iter
        (fun peer ->
          if peer <> me then (
            Connection_set.iter
              (fun connection ->
                let up =
                  match link_of net me connection with
                  | Some link ->
                      let link = Int_map.find link net.links in
                      link.up && number_at link me = connection
                  | None -> false
                in
                if not up then
                  failwith
                    (Format.asprintf "%a holds %a with %a, which is down" pp_peer_id me
                       pp_connection_id connection pp_peer_id peer))
              (N.connections p2p peer);
            if N.is_dialing p2p peer then
              failwith
                (Format.asprintf "%a is still dialing %a" pp_peer_id me pp_peer_id peer);
            let reported = Peer_map.find_opt peer causes.(i).reported in
            (match reported with
            | Some (_, true) ->
                failwith
                  (Format.asprintf "%a dialed %a and was never told how it ended"
                     pp_peer_id me pp_peer_id peer)
            | _ -> ());
            let told = match reported with Some (ready, _) -> ready | None -> false in
            if told <> N.is_ready p2p peer then
              failwith
                (Format.asprintf "%a was last told %a ready %b; it is ready %b" pp_peer_id
                   me pp_peer_id peer told (N.is_ready p2p peer))))
        ids)
    ids
