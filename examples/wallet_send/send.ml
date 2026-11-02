(* The wallet-send component.

   Its decisions depend on the rest of the node: whether the RPC is still
   pending, whether the chain and mempool still match an attempt's pins,
   which commitments the mempool already spends. A component sees its own
   State only, so that context arrives as data in its Actions ([start_plan],
   [collection_guard], the spent commitments, whether the RPC is pending),
   computed by the parent, which can read it. Everything it asks of the rest
   of the node leaves as an Output, which the parent lowers ([Node]). *)

open Causet
open Domain
open Mempool
open State

(* What [Start] found when it read the node. *)
type start_plan =
  | Noop
  | Reject of { req : send_request; error : string }
  | Collect_plan of {
      anchor_root : root;
      timestamp_ms : int;
      req : send_request;
      mempool_cursor : owned_outputs_cursor;
    } [@debug_name "Collect"]
[@@deriving debug]

(* Whether a collection may go on, given the node's RPC, chain and mempool. *)
type collection_guard =
  | Guard_continue [@debug_name "Continue"]  (** Everything still matches. *)
  | Retry  (** A pin went stale: start the attempt again. *)
  | Guard_noop [@debug_name "Noop"]  (** The RPC is gone or changed: drop the attempt. *)
[@@deriving debug]

type action =
  | Cancel of { rpc_id : rpc_id }
  | Start of { rpc_id : rpc_id; plan : start_plan }
  | Owned_chunk of {
      request : collect_request;
      result : (owned_utxos_chunk, string) result;
      guard : collection_guard;
      spent : Commitment_set.t;  (** The page's commitments the mempool already spends. *)
    }
  | Mempool_chunk of {
      request : mempool_request;
      guard : collection_guard;
      chunk : (owned_outputs_chunk, owned_outputs_read_error) result option;
          (** The page, read when the guard says to continue. *)
    }
  | Prepared of {
      request_id : Request_id.t;
      rpc_id : rpc_id;
      tx : submitted_tx;
      rpc_pending : bool;
    }
  | Proving_rejected of {
      request_id : Request_id.t;
      rpc_id : rpc_id;
      error : string;
      rpc_pending : bool;
    }
  | Rejected of { rpc_id : rpc_id; error : string }
[@@deriving message ~name:"wallet_send.action"]

(* What wallet send asks of the rest of the node, in order. *)
type output =
  | Collect of collect_request  (** Read one persisted page. *)
  | Scan_mempool of mempool_request  (** Read one mempool page now. *)
  | Prove of prove_request  (** Build, sign and prove the transaction. *)
  | Restart of { rpc_id : rpc_id }  (** Start the attempt again with a fresh plan. *)
  | Hydration_query of { rpc_id : rpc_id; anchor_root : root; selection_error : string }
  | Hydration_forget of { rpc_id : rpc_id }
  | Submit of submitted_tx
  | Respond of { rpc_id : rpc_id; response : send_response }
[@@deriving message ~name:"wallet_send.output"]

(* The checks on wallet send's own State; those that read the RPC are the
   parent's, since only it can read the RPC. *)
let is_enabled state action _ =
  match action with
  | Cancel { rpc_id } -> has_work state rpc_id
  | Start { rpc_id; _ } | Rejected { rpc_id; _ } -> can_start state rpc_id
  | Owned_chunk { request; _ } -> is_collecting_persisted state request
  | Mempool_chunk { request; _ } -> is_collecting_mempool state request
  | Prepared { request_id; rpc_id; _ } | Proving_rejected { request_id; rpc_id; _ } ->
      is_proving state rpc_id request_id

(* Ends the send with an error: hydration is forgotten, the RPC answered. *)
type ctx = (State.t, unit, action, never, output) Ctx.t

let terminal_rejection (ctx : ctx) rpc_id error =
  Ctx.emit_output ctx (Hydration_forget { rpc_id });
  Ctx.emit_output ctx (Respond { rpc_id; response = rejected error })

(* What a [Noop] or [Retry] guard does to the attempt: [true] when the
   reducer goes on. *)
let pass (ctx : ctx) rpc_id request_id guard =
  let state = Ctx.state ctx in
  match guard with
  | Guard_continue -> true
  | Guard_noop ->
      (match finish_collecting state rpc_id request_id with
      | Some state -> Ctx.set_state ctx (release_owner state rpc_id)
      | None -> ());
      false
  | Retry ->
      (match finish_collecting state rpc_id request_id with
      | Some state ->
          Ctx.set_state ctx state;
          Ctx.emit_output ctx (Restart { rpc_id })
      | None -> ());
      false

let reduce (ctx : ctx) action _ =
  match action with
  | Cancel { rpc_id } ->
      let state, had = cancel (Ctx.state ctx) rpc_id in
      Ctx.set_state ctx state;
      if had then Ctx.emit_output ctx (Hydration_forget { rpc_id })
  | Start { rpc_id; plan } -> (
      match plan with
      | Noop -> ()
      | Reject { error; _ } ->
          Ctx.set_state ctx (release_owner (Ctx.state ctx) rpc_id);
          terminal_rejection ctx rpc_id error
      | Collect_plan { anchor_root; timestamp_ms; req; mempool_cursor } -> (
          match reserve_owner (Ctx.state ctx) rpc_id req.from with
          | None ->
              Bug.condition ~fields:(rpc_field rpc_id) __POS__
                "wallet-send owner reservation changed during reduction"
          | Some state -> (
              Ctx.set_state ctx state;
              let request_id = Ctx.new_request ctx in
              match
                begin_collecting state request_id rpc_id anchor_root timestamp_ms req
                  mempool_cursor
              with
              | Ok (state, request) ->
                  Ctx.set_state ctx state;
                  Ctx.emit_output ctx (Collect request)
              | Error error ->
                  Ctx.set_state ctx (release_owner (not_ready state rpc_id) rpc_id);
                  terminal_rejection ctx rpc_id error)))
  | Owned_chunk { request; result; guard; spent } -> (
      if pass ctx request.rpc_id request.request_id guard then
        match apply_persisted_chunk (Ctx.state ctx) request result spent with
        | None ->
            Bug.condition ~fields:(rpc_field request.rpc_id) __POS__
              "wallet-send persisted chunk correlation changed during reduction"
        | Some (state, transition) -> (
            Ctx.set_state ctx state;
            match transition with
            | Collect next -> Ctx.emit_output ctx (Collect next)
            | Scan_mempool next -> Ctx.emit_output ctx (Scan_mempool next)
            | Hydrate { anchor_root; selection_error } ->
                Ctx.emit_output ctx
                  (Hydration_query
                     { rpc_id = request.rpc_id; anchor_root; selection_error })))
  | Mempool_chunk { request; guard; chunk } -> (
      if pass ctx request.rpc_id request.request_id guard then
        let rpc_id = request.rpc_id in
        match chunk with
        | None ->
            Bug.condition ~fields:(rpc_field rpc_id) __POS__
              "missing wallet-send mempool chunk context"
        | Some (Error Stale) -> (
            match finish_collecting (Ctx.state ctx) rpc_id request.request_id with
            | Some state ->
                Ctx.set_state ctx state;
                Ctx.emit_output ctx (Restart { rpc_id })
            | None -> ())
        | Some (Error error) -> (
            let shown = Debug.to_string pp_owned_outputs_read_error error in
            Bug.condition
              ~fields:(rpc_field rpc_id @ [ ("error", shown) ])
              __POS__ "wallet-send mempool owner scan failed";
            match finish_collecting (Ctx.state ctx) rpc_id request.request_id with
            | Some state ->
                Ctx.set_state ctx state;
                Ctx.emit_action ctx
                  (Rejected
                     { rpc_id; error = "wallet-send mempool owner scan failed: " ^ shown })
            | None -> ())
        | Some (Ok chunk) -> (
            match apply_mempool_chunk (Ctx.state ctx) request chunk with
            | None ->
                Bug.condition ~fields:(rpc_field rpc_id) __POS__
                  "wallet-send mempool chunk correlation changed during reduction"
            | Some (state, transition) -> (
                Ctx.set_state ctx state;
                match transition with
                | Continue -> ()
                | Prove prove -> Ctx.emit_output ctx (Prove prove)
                | Mempool_hydrate { anchor_root; selection_error } ->
                    Ctx.emit_output ctx
                      (Hydration_query { rpc_id; anchor_root; selection_error }))))
  | Prepared { request_id; rpc_id; tx; rpc_pending } -> (
      match finish_proving (Ctx.state ctx) rpc_id request_id with
      | None -> ()
      | Some state ->
          (* A deliberate limitation: the owner is released once the work is
             queued, before the mempool admits it. *)
          Ctx.set_state ctx (release_owner state rpc_id);
          if rpc_pending then (
            Ctx.emit_output ctx (Hydration_forget { rpc_id });
            Ctx.emit_output ctx (Submit tx);
            Ctx.emit_output ctx
              (Respond
                 {
                   rpc_id;
                   response = { queued = true; error = None; tx_id = Some tx.hash };
                 })))
  | Proving_rejected { request_id; rpc_id; error; rpc_pending } -> (
      match finish_proving (Ctx.state ctx) rpc_id request_id with
      | None -> ()
      | Some state ->
          Ctx.set_state ctx (release_owner state rpc_id);
          if rpc_pending then terminal_rejection ctx rpc_id error)
  | Rejected { rpc_id; error } ->
      Ctx.set_state ctx (release_owner (Ctx.state ctx) rpc_id);
      terminal_rejection ctx rpc_id error

let component : (State.t, unit, action, never, output) Component.t =
  Component.make ~name:"wallet_send" ~actions:action_message ~effects:Message.never
    ~outputs:output_message ~is_enabled ~reduce ()
