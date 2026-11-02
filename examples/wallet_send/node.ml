(* The light node around wallet send: the root component, which holds the
   rest of the node (the RPC requests, the chain's UTXO root, the mempool),
   reads it to give its children their context ([plan_start],
   [collection_guard], a hydration's peers and mempool spends), owns the
   node's own I/O, and lowers its children's Outputs into its own Actions.
   Its children are wallet send, partial-ledger sync (which hydrates a send
   that lacks data) and the network layer, [Peers]. *)

open Causet
open Domain
open State
module W = Send
module L = Partial_ledger
module P = Peers

(* An RPC's progress, as far as wallet send is concerned. *)
type rpc_status = Pending | Syncing  (** Waiting for wallet-spend hydration. *)
[@@deriving debug]

type rpc_request = { req : send_request; status : rpc_status } [@@deriving debug]

(* Which answer, by the request it answers. *)
type answer =
  | Page of Request_id.t  (** The next persisted page of a collection attempt. *)
  | Proof of Request_id.t  (** The proof of an attempt. *)
  | Hydration of Request_id.t  (** A hydration, by its query. *)
[@@deriving debug]

(* An answer a send waits for. *)
type awaited = { rpc_id : rpc_id; answer : answer } [@@deriving debug]

type state = {
  rpc : rpc_request Rpc_map.t;
      (** Pending wallet-send RPCs; answered ones are removed. *)
  chain_root : root;  (** The best tip's UTXO root. *)
  mempool : Mempool.t;
  wallet_send : State.t;
  ledger : L.state;  (** Partial-ledger sync: probes, the wallet cache, hydrations. *)
  peers : P.state;
  deadlines : awaited Pending.t;
      (** A deadline for every answer the node waits for, by the deadline's own ID: armed
          with each request it sends out, cleared by that answer. A send can wait for two
          at once (a hydration query still out while a restarted collection reads pages),
          so each has its own. *)
}

(* How long a send waits for any one answer (a page, a proof, a hydration)
   before it gives up. *)
let answer_timeout = 30_000_000_000

let create tip root =
  {
    rpc = Rpc_map.empty;
    chain_root = root;
    mempool = Mempool.create tip root;
    wallet_send = State.empty;
    ledger = L.empty;
    peers = P.empty;
    deadlines = Pending.empty;
  }

(* Whether the send's hydration is probing or storing. *)
let hydrating state rpc_id =
  match Rpc_map.find_opt rpc_id state.ledger.hydrations with
  | Some { stage = Probing _ | Storing _; _ } -> true
  | _ -> false

(* Whether the RPC is still waiting for its answer. *)
let rpc_pending state rpc_id = Rpc_map.mem rpc_id state.rpc

(* The children's invariants; that no send works while its hydration probes
   or stores; and that every probe out is a request the network has
   outstanding. *)
let invariants state =
  let ( let* ) = Result.bind in
  let* () = State.invariants state.wallet_send in
  let* () = L.invariants state.ledger in
  let* () = P.invariants state.peers in
  let* () =
    match List.find_opt (hydrating state) (working state.wallet_send) with
    | Some rpc_id ->
        Error (Format.asprintf "%a works while its hydration is out" pp_rpc_id rpc_id)
    | None -> Ok ()
  in
  List.fold_left
    (fun result (job, (probe : L.job)) ->
      let* () = result in
      match probe.phase with
      | L.Pending { request_id } ->
          let (L.Job_id j) = job in
          let outstanding =
            Option.map
              (fun (o : P.outstanding) -> (o.peer, o.correlation))
              (Request_map.find_opt request_id state.peers.outstanding)
          in
          if outstanding = Some (probe.peer, P.Correlation j) then Ok ()
          else
            Error
              (Format.asprintf "%a waits on %a, which the network does not have out"
                 L.pp_job_id job Request_id.pp request_id)
      | _ -> Ok ())
    (Ok ())
    (L.Job_map.bindings state.ledger.jobs)

(* ------------------------------------------------------------------------ *)
(* The node's Services: the UTXO database, the prover, the RPC transport,
   and partial-ledger sync's network, verifier and stores. Each answers
   through the Event it is given a Reply for. *)

type collected = {
  request : collect_request;
  result : (owned_utxos_chunk, string) result;
}
[@@deriving message ~name:"node.collected"]

type proved = {
  request_id : Request_id.t;
  rpc_id : rpc_id;
  result : (submitted_tx, string) result;
}
[@@deriving message ~name:"node.proved"]

(* The Services of every component of the node. Its children each see the
   part that is theirs. *)
type services = {
  collect : collect_request -> collected reply -> unit;
      (** The UTXO database: reads the proved UTXOs partial-ledger sync stored. *)
  prove : prove_request -> proved reply -> unit;
  respond : rpc_id -> send_response -> unit;
  send : P.peer_id -> Request_id.t -> Ledger.probe_request -> P.wire_answer reply -> unit;
  verify :
    L.job_id ->
    P.peer_id ->
    Request_id.t ->
    Ledger.probe_request ->
    Ledger.probe_response ->
    L.verify_done reply ->
    unit;
  store_cache :
    rpc_id -> Request_id.t -> root -> int list -> int list -> L.stored reply -> unit;
  store_partial :
    rpc_id -> Request_id.t -> root -> (int * utxo) list -> L.stored reply -> unit;
  timer : Timer.t;
      (** The answer deadlines, the network's timeouts, hydration's retries. *)
}

let peers_services (s : services) : P.services = { send = s.send; timer = s.timer }

let ledger_services (s : services) : L.services =
  {
    verify = s.verify;
    store_cache = s.store_cache;
    store_partial = s.store_partial;
    timer = s.timer;
  }

(* ------------------------------------------------------------------------ *)
(* The root *)

type action =
  | Send of { rpc_id : rpc_id; req : send_request }
      (** A wallet-send RPC arrives, and wallet send starts on it. *)
  | Start of { rpc_id : rpc_id }
  | Cancel of { rpc_id : rpc_id }
  | Check_timeouts  (** The heartbeat: at most one mempool page continues, round robin. *)
  | Expired of { deadline : Request_id.t }
      (** A deadline fired: if its send still waits on that answer, the send gives up. *)
  | Scan_mempool of { request : mempool_request }
  | Persisted_page of {
      request : collect_request;
      result : (owned_utxos_chunk, string) result;
    }
  | Proved of {
      request_id : Request_id.t;
      rpc_id : rpc_id;
      result : (submitted_tx, string) result;
    }
  | Collect of collect_request  (** Lowered from wallet send. *)
  | Prove of prove_request
  | Hydration_query of { rpc_id : rpc_id; anchor_root : root; selection_error : string }
  | Hydration_forget of { rpc_id : rpc_id }
  | Ledger of L.output  (** Lowered from partial-ledger sync and the network. *)
  | Peers of P.output
  | Submit of submitted_tx
  | Respond of { rpc_id : rpc_id; response : send_response }
  | Mempool_tx of { inputs : commitment list; outputs : utxo list }
      (** The world: a transaction enters the mempool, a new tip, the mempool alone moving
          to a new tip, an RPC whose client went away unanswered. *)
  | New_tip of { tip : block_hash; root : root }
  | Mempool_tip of { tip : block_hash }
  | Rpc_gone of { rpc_id : rpc_id }
      (** Test only: drops an RPC without answering it, which the node itself never does.
      *)
[@@deriving message ~name:"node.action"]

type effect_ =
  | Collect of collect_request
  | Prove of prove_request
  | Respond of { rpc_id : rpc_id; response : send_response }
  | Arm of { deadline : Request_id.t }
[@@deriving message ~name:"node.effect"]

let wallet_send_slot = Child.make "wallet_send" W.component
let ledger_slot = Child.make "ledger" L.component
let peers_slot = Child.make "peers" P.component

type ctx = (state, services, action, effect_, never) Ctx.t

(* What [Start] will do, read from the node. *)
let plan_start state rpc_id time : W.start_plan =
  match Rpc_map.find_opt rpc_id state.rpc with
  | None ->
      Bug.condition ~fields:(rpc_field rpc_id) __POS__
        "rpc not found in wallet-send start";
      Noop
  | Some rpc ->
      let req = rpc.req in
      (* One send per owner: a concurrent send is rejected rather than given
         its own inputs. *)
      if not (owner_is_available state.wallet_send rpc_id req.from) then
        Reject
          {
            error =
              Format.asprintf "wallet send already pending for owner %a" pp_owner req.from;
            req;
          }
      else if req.amount = 0 then
        Reject { req; error = "amount must be greater than zero" }
      else if checked_add req.amount (Option.value req.fee ~default:0) = None then
        Reject { req; error = "amount+fee overflow" }
      else
        Collect_plan
          {
            anchor_root = state.chain_root;
            timestamp_ms = time / 1_000_000;
            mempool_cursor = Mempool.begin_owned_scan state.mempool req.from;
            req;
          }

(* Whether a collection may go on, read from the node. *)
let collection_guard state rpc_id request_id : W.collection_guard =
  match Rpc_map.find_opt rpc_id state.rpc with
  | None -> Guard_noop
  | Some rpc -> (
      if not (collection_matches_rpc_request state.wallet_send rpc_id request_id rpc.req)
      then Guard_noop
      else
        match collection_pins state.wallet_send rpc_id request_id with
        | None -> Guard_noop
        | Some (anchor_root, cursor) ->
            if
              state.chain_root <> anchor_root
              || Mempool.tip_root state.mempool <> anchor_root
              || Result.is_error (Mempool.validate_cursor state.mempool cursor)
            then Retry
            else Guard_continue)

(* A hydration query, with what partial-ledger sync cannot read itself: the
   send's owner and amount, whether it is pending, the peers serving its
   anchor root, and which of the owner's known UTXOs the mempool spends. *)
let hydration_query state rpc_id anchor_root selection_error : L.action =
  let rpc = Rpc_map.find_opt rpc_id state.rpc in
  let owner, required =
    match rpc with
    | None -> (Owner 0, 0)
    | Some rpc ->
        let fee = Option.value rpc.req.fee ~default:0 in
        (rpc.req.from, saturating_add rpc.req.amount fee)
  in
  let spent =
    match Owner_map.find_opt owner state.ledger.cache with
    | None -> Commitment_set.empty
    | Some entries ->
        Int_map.fold
          (fun _ (entry : L.cache_entry) s ->
            if Mempool.has_spend state.mempool entry.commitment then
              Commitment_set.add entry.commitment s
            else s)
          entries Commitment_set.empty
  in
  Query
    {
      rpc_id;
      anchor_root;
      selection_error;
      owner;
      required;
      rpc_pending = rpc <> None;
      serving = P.serving state.peers anchor_root;
      spent;
    }

(* Arms a deadline for an answer [rpc_id] now waits for. *)
let await_answer (ctx : ctx) rpc_id answer =
  let deadline = Ctx.new_request ctx in
  Ctx.update ctx (fun s ->
      { s with deadlines = Pending.add deadline { rpc_id; answer } s.deadlines });
  deadline

(* Whether the send still waits for this answer: the attempt that asked is
   still the send's current one. *)
let still_awaited state { rpc_id; answer } =
  match answer with
  | Page attempt -> collection_pins state.wallet_send rpc_id attempt <> None
  | Proof attempt -> is_proving state.wallet_send rpc_id attempt
  | Hydration query -> L.hydration_query state.ledger rpc_id = Some query

let drop_deadlines state keep =
  {
    state with
    deadlines = Pending.retain (fun _ awaited -> keep awaited) state.deadlines;
  }

(* An awaited answer came: its deadline goes, and fires stale. *)
let answered state answer = drop_deadlines state (fun a -> a.answer <> answer)

(* A send's hydration ended: its deadline goes. *)
let hydration_ended state rpc_id =
  drop_deadlines state (fun a ->
      a.rpc_id <> rpc_id || match a.answer with Hydration _ -> false | _ -> true)

(* The enabling checks that read the RPC and wallet send. *)
let is_enabled state (action : action) _ =
  let ws = state.wallet_send in
  match action with
  | Send { rpc_id; _ } -> not (Rpc_map.mem rpc_id state.rpc)
  (* Not while the send's probe is out or its results are being stored: a
     collection started then would outlive the hydration, whose own retry
     would then be refused. *)
  | Start { rpc_id } ->
      rpc_pending state rpc_id && can_start ws rpc_id && not (hydrating state rpc_id)
  | Cancel { rpc_id } -> has_work ws rpc_id
  | Scan_mempool { request } -> is_collecting_mempool ws request
  | Persisted_page { request; _ } -> is_collecting_persisted ws request
  | Proved { request_id; rpc_id; _ } -> is_proving ws rpc_id request_id
  | _ -> true

let to_wallet_send (ctx : ctx) action =
  ignore (Ctx.reduce_child ctx wallet_send_slot action)

let to_ledger (ctx : ctx) action = ignore (Ctx.reduce_child ctx ledger_slot action)

let reduce (ctx : ctx) (action : action) (meta : Meta.t) =
  let state = Ctx.state ctx in
  match action with
  | Send { rpc_id; req } ->
      Ctx.set_state ctx
        { state with rpc = Rpc_map.add rpc_id { req; status = Pending } state.rpc };
      Ctx.emit_action ctx (Start { rpc_id })
  | Start { rpc_id } ->
      let plan = plan_start state rpc_id meta.time in
      to_wallet_send ctx (W.Start { rpc_id; plan })
  | Cancel { rpc_id } ->
      (* Nothing of the cancelled work is awaited any more. *)
      Ctx.set_state ctx (drop_deadlines state (fun a -> a.rpc_id <> rpc_id));
      to_wallet_send ctx (W.Cancel { rpc_id })
  | Check_timeouts -> (
      match next_mempool_scan_request state.wallet_send with
      | Ok (Some request) -> Ctx.emit_action ctx (Scan_mempool { request })
      | Ok None -> ()
      | Error rpc_id ->
          Bug.condition ~fields:(rpc_field rpc_id) __POS__
            "wallet-send mempool scan readiness is inconsistent")
  | Scan_mempool { request } ->
      let guard = collection_guard state request.rpc_id request.request_id in
      let chunk =
        if guard = Guard_continue then
          Some
            (Mempool.owned_outputs_chunk state.mempool request.cursor
               request.max_scan_positions request.max_matches)
        else None
      in
      to_wallet_send ctx (W.Mempool_chunk { request; guard; chunk })
  | Persisted_page { request; result } ->
      let state = answered state (Page request.request_id) in
      Ctx.set_state ctx state;
      let guard = collection_guard state request.rpc_id request.request_id in
      let spent =
        match result with
        | Ok chunk
          when guard = Guard_continue
               && Result.is_ok (validate_persisted_chunk request chunk) ->
            List.fold_left
              (fun s (c, _) ->
                if Mempool.has_spend state.mempool c then Commitment_set.add c s else s)
              Commitment_set.empty chunk.utxos
        | _ -> Commitment_set.empty
      in
      to_wallet_send ctx (W.Owned_chunk { request; result; guard; spent })
  | Proved { request_id; rpc_id; result } ->
      let state = answered state (Proof request_id) in
      Ctx.set_state ctx state;
      let rpc_pending = rpc_pending state rpc_id in
      to_wallet_send ctx
        (match result with
        | Ok tx -> W.Prepared { request_id; rpc_id; tx; rpc_pending }
        | Error error -> W.Proving_rejected { request_id; rpc_id; error; rpc_pending })
  | Collect request ->
      let deadline = await_answer ctx request.rpc_id (Page request.request_id) in
      Ctx.emit_effect ctx (Collect request);
      Ctx.emit_effect ctx (Arm { deadline })
  | Prove request ->
      let deadline = await_answer ctx request.rpc_id (Proof request.request_id) in
      Ctx.emit_effect ctx (Prove request);
      Ctx.emit_effect ctx (Arm { deadline })
  | Expired { deadline } -> (
      match Pending.take deadline state.deadlines with
      (* An answer that came in time cleared its deadline: stale. *)
      | None -> ()
      | Some (awaited, deadlines) ->
          let state = { state with deadlines } in
          Ctx.set_state ctx state;
          (* So is one whose attempt has ended since, whatever ended it. *)
          if still_awaited state awaited then (
            let rpc_id = awaited.rpc_id in
            Ctx.set_state ctx (drop_deadlines state (fun a -> a.rpc_id <> rpc_id));
            to_wallet_send ctx (W.Cancel { rpc_id });
            Ctx.emit_action ctx
              (Respond { rpc_id; response = rejected "timed out waiting for an answer" }))
      )
  | Hydration_query { rpc_id; anchor_root; selection_error } ->
      to_ledger ctx (hydration_query state rpc_id anchor_root selection_error)
  | Hydration_forget { rpc_id } -> to_ledger ctx (L.Forget { rpc_id })
  | Ledger output -> (
      match output with
      | L.Request { job = Job_id j; peer; request } ->
          ignore
            (Ctx.reduce_child ctx peers_slot
               (P.Request { peer; correlation = Correlation j; request }))
      (* A hydration began: the send is syncing, and waits for it at most
         [answer_timeout]. *)
      | Syncing { rpc_id; query } ->
          (match Rpc_map.find_opt rpc_id state.rpc with
          | Some rpc ->
              Ctx.set_state ctx
                {
                  state with
                  rpc = Rpc_map.add rpc_id { rpc with status = Syncing } state.rpc;
                }
          | None -> ());
          let deadline = await_answer ctx rpc_id (Hydration query) in
          Ctx.emit_effect ctx (Arm { deadline })
      | Retry { rpc_id } ->
          let state = hydration_ended state rpc_id in
          Ctx.set_state ctx state;
          if rpc_pending state rpc_id then Ctx.emit_action ctx (Start { rpc_id })
      | Rejected { rpc_id; error } ->
          let state = hydration_ended state rpc_id in
          Ctx.set_state ctx state;
          if rpc_pending state rpc_id then
            to_wallet_send ctx (W.Rejected { rpc_id; error }))
  | Peers output ->
      to_ledger ctx
        (match output with
        | P.Started { correlation = Correlation c; peer; request_id; request } ->
            L.Started { job = Job_id c; peer; request_id; request }
        | Answered { correlation = Correlation c; peer; request_id; answer } ->
            L.Answered { job = Job_id c; peer; request_id; answer }
        | Failed { correlation = Correlation c; peer; request_id; error } ->
            L.Failed { job = Job_id c; peer; request_id; error }
        | Refused { correlation = Correlation c; peer } ->
            L.Refused { job = Job_id c; peer }
        | Gone { peer } -> L.Peer_gone { peer })
  | Submit tx ->
      Ctx.set_state ctx { state with mempool = Mempool.submit state.mempool tx }
  (* The answer finishes the RPC: the request goes, and wallet send is told
     to cancel whatever it still holds for it. *)
  | Respond { rpc_id; response } ->
      let state = drop_deadlines state (fun a -> a.rpc_id <> rpc_id) in
      if Rpc_map.mem rpc_id state.rpc then (
        Ctx.set_state ctx { state with rpc = Rpc_map.remove rpc_id state.rpc };
        Ctx.emit_effect ctx (Respond { rpc_id; response });
        Ctx.emit_action ctx (Cancel { rpc_id }))
      else Ctx.set_state ctx state
  | Mempool_tx { inputs; outputs } ->
      Ctx.set_state ctx
        { state with mempool = Mempool.accept state.mempool inputs outputs }
  | New_tip { tip; root } ->
      let mempool = Mempool.with_tip_root (Mempool.set_tip state.mempool tip) root in
      Ctx.set_state ctx { state with chain_root = root; mempool }
  | Mempool_tip { tip } ->
      Ctx.set_state ctx { state with mempool = Mempool.set_tip state.mempool tip }
  | Rpc_gone { rpc_id } ->
      Ctx.set_state ctx { state with rpc = Rpc_map.remove rpc_id state.rpc }

let handle ctx (eff : effect_) _ =
  let service : services = Effect_ctx.service ctx in
  match eff with
  | Collect request -> service.collect request (Effect_ctx.reply ctx collected_message)
  | Prove request -> service.prove request (Effect_ctx.reply ctx proved_message)
  | Respond { rpc_id; response } -> service.respond rpc_id response
  | Arm { deadline } ->
      let reply = Effect_ctx.reply ctx Timer.fired in
      Timer.after service.timer ~delay:answer_timeout reply deadline

(* Wallet send's Outputs become the node's Actions, in order. *)
let lower_wallet_send _ (output : W.output) port =
  Port.emit_action port
    (match output with
    | Collect request -> (Collect request : action)
    | Scan_mempool request -> Scan_mempool { request }
    | Prove request -> Prove request
    | Restart { rpc_id } -> Start { rpc_id }
    | Hydration_query { rpc_id; anchor_root; selection_error } ->
        Hydration_query { rpc_id; anchor_root; selection_error }
    | Hydration_forget { rpc_id } -> Hydration_forget { rpc_id }
    | Submit tx -> Submit tx
    | Respond { rpc_id; response } -> Respond { rpc_id; response })

let component : (state, services, action, effect_, never) Component.t =
  Component.make ~name:"node" ~actions:action_message ~effects:effect_message
    ~outputs:Message.never ~is_enabled ~reduce ~handle
    ~register:(fun r ->
      Registrar.child r wallet_send_slot
        ~lens:
          (Lens.field
             ~get:(fun s -> s.wallet_send)
             ~set:(fun s wallet_send -> { s with wallet_send }))
        ~on_output:lower_wallet_send ();
      Registrar.child r ledger_slot
        ~lens:
          (Lens.field ~get:(fun s -> s.ledger) ~set:(fun s ledger -> { s with ledger }))
        ~service:ledger_services
        ~on_output:(fun _ output port -> Port.emit_action port (Ledger output))
        ();
      Registrar.child r peers_slot
        ~lens:(Lens.field ~get:(fun s -> s.peers) ~set:(fun s peers -> { s with peers }))
        ~service:peers_services
        ~on_output:(fun _ output port -> Port.emit_action port (Peers output))
        ();
      Registrar.event r collected_message (fun (event : collected) port ->
          Port.emit_action port
            (Persisted_page { request = event.request; result = event.result }));
      Registrar.event r proved_message (fun (event : proved) port ->
          Port.emit_action port
            (Proved
               {
                 request_id = event.request_id;
                 rpc_id = event.rpc_id;
                 result = event.result;
               }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Expired { deadline = fired.request })))
    ()
