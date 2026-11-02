(* Simulated connections, transport and server Services. *)

open Causet
open P2p
open P2p_node
open Sim_common

(* ------------------------------------------------------------------------ *)
(* The network *)

(* One connection between two nodes, known at each end by that end's own
   number: the dialer first, then the end that accepted it (numbered once
   it is). *)
type link = {
  mutable ends : (peer_id * connection_id) * (peer_id * connection_id);
  mutable up : bool;
}

(* The other end, as [me] sees it: the peer, and its own number. *)
let other_end link me =
  let first, second = link.ends in
  if fst first = me then second else first

let number_at link node =
  let first, second = link.ends in
  if fst first = node then snd first else snd second

type msg =
  | To_p2p of { to_ : peer_id; event : N.transport_event }
  | Served_msg of { reply : served reply; event : served }
  | Establish of { link : int; dial : Dial_id.t }
      (** A dial connects: the end dialed accepts it. *)

type p2p_mount = (N.state, N.services, N.action, N.effect_, N.output) mount
type node_mount = (node_state, node_services, node_action, node_effect, never) mount

type net = {
  world : Rng.t;
  clock : Clock.Manual.m;
  mutable nodes : (Sink.t * p2p_mount) Peer_map.t;
  mutable links : link Int_map.t;
  mutable next_link : int;
  mutable numbers : int Peer_map.t;  (** Each node's own count of its connections. *)
  mutable by_end : int End_map.t;  (** The link each node's connection is. *)
  flight : (msg, int * peer_id) In_flight.t;
      (** In order on each link, each way: [(link, from)]. *)
  delivered : (peer_id * N.transport_event) Dynarray.t;
      (** Callbacks delivered, for replaying stale ones. *)
  mutable requests_in : Request_set.t;
      (** The requests each node received, by link and wire ID: an answer goes back on its
          request's link. *)
  mutable cut : Int_set.t;
      (** Dials in flight that fail when due: the pair was cut off. *)
  mutable violations : string list;  (** newest first *)
}

let number net node =
  let next = Option.value (Peer_map.find_opt node net.numbers) ~default:0 + 1 in
  net.numbers <- Peer_map.add node next net.numbers;
  Connection_id next

(* A callback to [to_], from the far end of [link], after a link delay, in
   order. *)
let deliver net link from to_ event =
  let at = Clock.Manual.get net.clock + ms + (Rng.below net.world 20 * ms) in
  ignore (In_flight.send_in_order net.flight (link, from) at (To_p2p { to_; event }))

let link_of net node connection = End_map.find_opt (node, connection) net.by_end

let is_up net node connection =
  match link_of net node connection with
  | Some link -> (Int_map.find link net.links).up
  | None -> false

(* The dialed end accepts the link, numbering it now: both ends hear it
   connected. *)
let establish net link dial =
  let established = Int_map.find link net.links in
  let (dialer, number_), (accepter, _) = established.ends in
  let accepted = number net accepter in
  established.ends <- ((dialer, number_), (accepter, accepted));
  established.up <- true;
  net.by_end <- End_map.add (accepter, accepted) link net.by_end;
  deliver net link accepter dialer
    (N.Connected { peer = accepter; connection = number_; dial = Some dial });
  deliver net link dialer accepter
    (N.Connected { peer = dialer; connection = accepted; dial = None })

(* The network opens another connection between [a] and [b] on its own:
   each end numbers it, and hears it opened by the other. *)
let open_extra net a b =
  let link = net.next_link in
  net.next_link <- link + 1;
  let at_a = number net a in
  let at_b = number net b in
  net.links <- Int_map.add link { ends = ((a, at_a), (b, at_b)); up = true } net.links;
  net.by_end <- End_map.add (a, at_a) link net.by_end;
  net.by_end <- End_map.add (b, at_b) link net.by_end;
  deliver net link b a (N.Connected { peer = b; connection = at_a; dial = None });
  deliver net link a b (N.Connected { peer = a; connection = at_b; dial = None })

(* The links up between [a] and [b]. *)
let links_between net a b =
  Int_map.fold
    (fun id link acc ->
      let (x, _), (y, _) = link.ends in
      let ends = [ x; y ] in
      if link.up && List.mem a ends && List.mem b ends then id :: acc else acc)
    net.links []
  |> List.rev

(* [from] sends [to_] six requests at once, spread over their links, as a
   peer that does not keep to one request at a time would (wire IDs from
   1,000, past any its P2P uses here); and, on every link, answers nobody
   asked for, under wire IDs 0 to 2. Those carry no value: one on the
   connection and wire ID of [to_]'s request out is, to P2P, that request's
   answer. *)
let flood net from to_ =
  let links = links_between net from to_ in
  if links <> [] then (
    for n = 0 to 5 do
      let link = List.nth links (n mod List.length links) in
      let _, theirs = other_end (Int_map.find link net.links) from in
      let request : req = { key = 10 + n; tag = 1_000 + n } in
      let message = N.Request { wire = Wire_id (1_000 + n); request } in
      deliver net link from to_ (N.Received { peer = from; connection = theirs; message })
    done;
    List.iter
      (fun link ->
        let _, theirs = other_end (Int_map.find link net.links) from in
        for wire = 0 to 2 do
          let message = N.Response { wire = Wire_id wire; response = None } in
          deliver net link from to_
            (N.Received { peer = from; connection = theirs; message })
        done)
      links)

(* Every dial between [a] and [b] still in flight fails when due. *)
let cut_dials net a b =
  Int_map.iter
    (fun id link ->
      let (dialer, _), (dialed, accepted) = link.ends in
      let between = (dialer = a && dialed = b) || (dialer = b && dialed = a) in
      if between && (not link.up) && accepted = Connection_id 0 then
        net.cut <- Int_set.add id net.cut)
    net.links

(* The link breaks: both ends hear it closed. *)
let break_link net link =
  match Int_map.find_opt link net.links with
  | Some broken when broken.up ->
      broken.up <- false;
      let (a, at_a), (b, at_b) = broken.ends in
      deliver net link b a (N.Closed { peer = b; connection = at_a });
      deliver net link a b (N.Closed { peer = a; connection = at_b })
  | _ -> ()

(* A callback delivered before, delivered again now: with [responses], only
   an old response, the stale callback that could answer a current request
   reusing its wire ID. Otherwise also an old message, close or failure, or
   a dial's failure; never a [Connected], which the transport reports
   once. *)
let replay_stale net pick responses =
  let stale =
    Dynarray.to_list net.delivered
    |> List.filter (fun (to_, (event : N.transport_event)) ->
           match event with
           | Received { connection; message = Response _; _ } ->
               not (is_up net to_ connection)
           | _ when responses -> false
           | Received { connection; _ }
           | Closed { connection; _ }
           | Send_failed { connection; _ } ->
               not (is_up net to_ connection)
           | Dial_failed _ -> true
           | Connected _ -> false)
  in
  if stale <> [] then
    let to_, event = List.nth stale (pick mod List.length stale) in
    In_flight.send_at net.flight (Clock.Manual.get net.clock + ms) (To_p2p { to_; event })

let advance net now =
  List.iter
    (function
      | To_p2p { to_; event } ->
          (match event with
          | Received { connection; message = Request { wire; _ }; _ } -> (
              match link_of net to_ connection with
              | Some link ->
                  net.requests_in <- Request_set.add (link, to_, wire) net.requests_in
              | None -> ())
          | _ -> ());
          Dynarray.add_last net.delivered (to_, event);
          let sink, mount = Peer_map.find to_ net.nodes in
          ignore (Sink.try_send_to_as_worker sink mount N.transport_event_message event)
      | Served_msg { reply; event } -> ignore (Reply.try_send_as_worker reply event)
      | Establish { link; dial } when Int_set.mem link net.cut ->
          net.cut <- Int_set.remove link net.cut;
          let (dialer, _), (peer, _) = (Int_map.find link net.links).ends in
          deliver net link peer dialer (N.Dial_failed { peer; dial })
      | Establish { link; dial } -> establish net link dial)
    (In_flight.due net.flight now)

let world net =
  {
    Causet_test.next_time = (fun () -> In_flight.next_time net.flight);
    advance = advance net;
  }

(* One node's transport, on the shared network. *)
let transport net me : N.transport =
  {
    (* Numbered now; connected 1 to 30 ms later, one in ten failing. *)
    dial =
      (fun peer dial ->
        if (not (Peer_map.mem peer net.nodes)) || Rng.chance net.world 1 10 then
          let at = Clock.Manual.get net.clock + ms in
          In_flight.send_at net.flight at
            (To_p2p { to_ = me; event = N.Dial_failed { peer; dial } })
        else
          let link = net.next_link in
          net.next_link <- link + 1;
          let number_ = number net me in
          net.links <-
            Int_map.add link
              { ends = ((me, number_), (peer, Connection_id 0)); up = false }
              net.links;
          net.by_end <- End_map.add (me, number_) link net.by_end;
          let at = Clock.Manual.get net.clock + ms + (Rng.below net.world 30 * ms) in
          In_flight.send_at net.flight at (Establish { link; dial }));
    (* Lost one in thirty; failed at once one in thirty, and always on a
       connection that is not up. *)
    send =
      (fun peer connection message ->
        let sent = N.sent_of message in
        let link = link_of net me connection in
        (match (sent, link) with
        | Sent_response wire, Some on
          when not (Request_set.mem (on, me, wire) net.requests_in) ->
            net.violations <-
              Format.asprintf "%a answered %a on %a, not its request's" pp_peer_id me
                pp_wire_id wire pp_connection_id connection
              :: net.violations
        | _ -> ());
        let up = is_up net me connection in
        if (not up) || Rng.chance net.world 1 30 then
          let failed = N.Send_failed { peer; connection; sent } in
          match link with
          | Some link -> deliver net link peer me failed
          | None ->
              let at = Clock.Manual.get net.clock + ms in
              In_flight.send_at net.flight at (To_p2p { to_ = me; event = failed })
        else if Rng.chance net.world 1 30 then ()
        else
          let link = Option.get link in
          let _, theirs = other_end (Int_map.find link net.links) me in
          deliver net link me peer
            (N.Received { peer = me; connection = theirs; message }));
    close =
      (fun peer connection ->
        match link_of net me connection with
        | None -> ()
        | Some link ->
            let closed = Int_map.find link net.links in
            if closed.up then (
              closed.up <- false;
              let _, theirs = other_end closed me in
              deliver net link me peer (N.Closed { peer = me; connection = theirs })));
  }

(* One node's server: answers after up to 3 s, past a disconnect sometimes;
   one in five after 4 to 8 s, past the requester's timeout; one in ten
   never. *)
let server net me =
  {
    serve =
      (fun token request reply ->
        if not (Rng.chance net.world 1 10) then
          let after =
            if Rng.chance net.world 1 5 then 4_000 + Rng.below net.world 4_000
            else Rng.below net.world 3_000
          in
          let at = Clock.Manual.get net.clock + ms + (after * ms) in
          let event = { token; response = Some (resp_of me request) } in
          In_flight.send_at net.flight at (Served_msg { reply; event }));
  }
