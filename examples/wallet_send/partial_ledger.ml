(* Partial-ledger sync: the light node asks full peers for some leaves of the
   ledger, with proofs, and uses them without holding the whole ledger.

   Two parts share this component:

   - The probe lifecycle, shared by every consumer. A job is admitted (at
     most one per peer, whatever its phase; at most [max_in_flight]), then
     goes [Init] -> [Pending] (its request is out, under the network's
     request ID) -> [Verify_pending] (answered, its proofs being checked).
     Four identities are kept apart: the job ID, the peer, the network's
     request ID and the exact request; a report that does not match all
     that apply is stale. Every ending, success or failure, is a
     [probe_result] that keeps the job's [target], and one exhaustive match
     on the target routes it ([route]). On a disconnect, the peer's [Init]
     and [Pending] jobs fail; [Verify_pending] ones are kept, since the
     exchange is over and verification needs no peer. Verification is the
     one wait nothing else bounds, so it runs only for a result its
     consumer still waits for: an answer that comes after is not verified,
     and a consumer that stops waiting retires a job being verified for it.
   - Wallet-spend hydration, its one consumer here. When a send lacks funds
     in what the node has stored, it picks candidates from the wallet cache
     (UTXOs the wallet knows of, by leaf, not yet checked at the anchor
     root, not spent in the mempool), probes a peer serving that root,
     records what the probe proved in two stores, waits for both, in either
     order, and asks for the send to be tried again. Each write names the
     hydration (its query), so a cancelled hydration's acknowledgement
     cannot end the one that replaced it; and a cache entry is held as
     checked only once the store that makes it durable has acknowledged.

   The network is the [Peers] sibling: requests leave as Outputs, its
   reports arrive as Actions from the root, which also passes in what this
   component cannot read (which peers serve the anchor root, which
   commitments the mempool spends, whether the send is still pending). *)

open Causet
open Domain
open Ledger
open Peers

(* At most this many probes at once. *)
let max_in_flight = 4

(* A hydration probes at most this many leaves. *)
let max_items = 64

(* How long a hydration waits before the send is tried again, when it could
   not probe yet. *)
let retry_cooldown = 500_000_000

(* A probe job. *)
type job_id = Job_id of Request_id.t [@@unboxed] [@@deriving debug]

module Job_map = Map.Make (struct
  type t = job_id

  let compare (Job_id a) (Job_id b) = Request_id.compare a b
end)

(* Where a job's result goes: routing data, kept through every ending. *)
type target = Spend_hydration of { rpc_id : rpc_id } [@@deriving debug]

type job_phase =
  | Init  (** Admitted; the request is not out yet. *)
  | Pending of { request_id : Request_id.t }
      (** The request is out under the network's [request_id]. *)
  | Verify_pending of { request_id : Request_id.t }
      (** Answered; the proofs are being verified. *)
[@@deriving debug]

type job = { target : target; peer : peer_id; request : probe_request; phase : job_phase }
[@@deriving debug]

(* Why a probe failed. *)
type sync_error =
  | Rejected of string
      (** Not admitted, or the peer could not take it: nothing was asked. *)
  | Disconnected
  | Timeout
  | Unavailable  (** The peer does not have the ledger at that root. *)
  | Mismatch of string  (** The answer, or its verification, is not for this exchange. *)
  | Bad_proof of string
  | Abandoned  (** Answered after its consumer stopped waiting: not verified. *)
[@@deriving debug]

(* How a job ended. *)
type probe_result = {
  job : job_id;
  target : target;
  peer : peer_id;
  request : probe_request;
  result : (verified, sync_error) result;
}

(* A UTXO the wallet knows of, by leaf. *)
type cache_entry = {
  commitment : commitment;
  amount : int;
  checked : (root * bool) option;
      (** The last root a probe checked it at, and whether it was there. *)
}
[@@deriving debug]

type stage =
  | Probing of { job : job_id }
  | Storing of {
      awaiting_partial : bool;
      awaiting_cache : bool;
      found : index_ref list;
      missing : index_ref list;
    }
      (** The probe's results are being stored: which stores have not acknowledged yet,
          and the cache entries each one's acknowledgement marks as checked. A leaf found
          is marked only once the partial store holds it: until then, a later probe may
          find it again. *)
  | Waiting of { retry : Request_id.t }
      (** Could not probe yet; the send is tried again when [retry] fires. *)
[@@deriving debug]

(* One send's hydration, from the query that began it to its end. *)
type hydration = {
  query : Request_id.t;
      (** Names this hydration to the root, which bounds it with a deadline. *)
  owner : owner;
  anchor_root : root;
  stage : stage;
}
[@@deriving debug]

type state = {
  jobs : job Job_map.t;
  cache : cache_entry Int_map.t Owner_map.t;
  seeded : bool;
      (** Whether the wallet cache has been filled once. Until then, a send that lacks
          funds waits rather than being rejected. *)
  attempted : Rpc_set.t;
      (** Sends that have probed since they last failed for good: one probe per send. *)
  hydrations : hydration Rpc_map.t;
}

let empty =
  {
    jobs = Job_map.empty;
    cache = Owner_map.empty;
    seeded = false;
    attempted = Rpc_set.empty;
    hydrations = Rpc_map.empty;
  }

let has_job state peer = Job_map.exists (fun _ (job : job) -> job.peer = peer) state.jobs

(* The query of the send's current hydration. *)
let hydration_query state rpc_id =
  Option.map (fun h -> h.query) (Rpc_map.find_opt rpc_id state.hydrations)

(* Forgotten: no hydration, and no probe counted against the send. *)
let forgotten state rpc_id =
  (not (Rpc_set.mem rpc_id state.attempted)) && not (Rpc_map.mem rpc_id state.hydrations)

(* The cache entries a probe has checked: [(root, leaf, commitment,
   present)]. *)
let checked state =
  Owner_map.bindings state.cache
  |> List.concat_map (fun (_, entries) ->
         Int_map.bindings entries
         |> List.filter_map (fun (index, entry) ->
                Option.map
                  (fun (root, present) -> (root, index, entry.commitment, present))
                  entry.checked))

(* The invariants of the lifecycle and of hydration. *)
let invariants state =
  let ( let* ) = Result.bind in
  let rec all f = function
    | [] -> Ok ()
    | x :: rest ->
        let* () = f x in
        all f rest
  in
  let jobs = Job_map.bindings state.jobs in
  let* () =
    if List.length jobs > max_in_flight then
      Error (Printf.sprintf "%d jobs in flight" (List.length jobs))
    else Ok ()
  in
  let* _ =
    List.fold_left
      (fun seen (_, (job : job)) ->
        let* seen = seen in
        if Peer_map.mem job.peer seen then
          Error (Format.asprintf "two jobs for %a" pp_peer_id job.peer)
        else Ok (Peer_map.add job.peer () seen))
      (Ok Peer_map.empty) jobs
  in
  (* A job still out on the network can outlive its hydration, forgotten
     since: the network ends it, and its result is a no-op. A job being
     verified cannot: nothing else would end it. *)
  let* () =
    all
      (fun (id, (job : job)) ->
        let (Spend_hydration { rpc_id }) = job.target in
        let stage =
          Option.map (fun h -> h.stage) (Rpc_map.find_opt rpc_id state.hydrations)
        in
        match job.phase with
        | Verify_pending _ when stage <> Some (Probing { job = id }) ->
            Error (Format.asprintf "%a is verified for no hydration" pp_job_id id)
        | _ -> Ok ())
      jobs
  in
  all
    (fun (rpc_id, h) ->
      match h.stage with
      | Probing { job } -> (
          match Job_map.find_opt job state.jobs with
          | None ->
              Error
                (Format.asprintf "%a probes with %a, which is gone" pp_rpc_id rpc_id
                   pp_job_id job)
          | Some probe ->
              if probe.target <> Spend_hydration { rpc_id } then
                Error
                  (Format.asprintf "%a probes with another's %a" pp_rpc_id rpc_id
                     pp_job_id job)
              else if probe.request.root <> h.anchor_root then
                Error (Format.asprintf "%a probes another root" pp_rpc_id rpc_id)
              else Ok ())
      | Storing { awaiting_partial; awaiting_cache; _ } ->
          if (not awaiting_partial) && not awaiting_cache then
            Error (Format.asprintf "%a stored but not retried" pp_rpc_id rpc_id)
          else Ok ()
      | Waiting _ -> Ok ())
    (Rpc_map.bindings state.hydrations)

(* The candidates a hydration probes: the owner's cache entries not yet
   checked at [root] and not spent in the mempool, the newest first, until
   they cover [required], at most [max_items]. *)
let candidates state owner root spent required =
  match Owner_map.find_opt owner state.cache with
  | None -> []
  | Some entries ->
      let rec go total items = function
        | [] -> List.rev items
        | _ when total >= required || List.length items = max_items -> List.rev items
        | (index, entry) :: rest ->
            let checked_here =
              match entry.checked with Some (at, _) -> at = root | None -> false
            in
            if checked_here || Commitment_set.mem entry.commitment spent then
              go total items rest
            else
              go
                (saturating_add total entry.amount)
                ({ index; expected = entry.commitment } :: items)
                rest
      in
      go 0 [] (List.rev (Int_map.bindings entries))

(* ------------------------------------------------------------------------ *)
(* Services: the proof verifier, and the two stores a hydration writes. *)

type verify_done = {
  job : job_id;
  peer : peer_id;
  request_id : Request_id.t;
  request : probe_request;
  result : (verified, string) result;
}
[@@deriving message ~name:"ledger.verified"]

(* A store's acknowledgement, naming the hydration whose write it was. *)
type stored =
  | Cache of { rpc_id : rpc_id; query : Request_id.t; root : root; ok : bool }
      (** The wallet cache's record of what the probe checked. *)
  | Partial of { rpc_id : rpc_id; query : Request_id.t; root : root; ok : bool }
      (** The proved UTXOs, which the UTXO database reads. *)
[@@deriving message ~name:"ledger.stored"]

(* The verifier verifies the response's proofs ([Ledger.verify_response])
   and answers with the request's identities. Each store write names the
   hydration it is for ([rpc_id], [query]), which its acknowledgement
   carries back. *)
type services = {
  verify :
    job_id ->
    peer_id ->
    Request_id.t ->
    probe_request ->
    probe_response ->
    verify_done reply ->
    unit;
  store_cache :
    rpc_id -> Request_id.t -> root -> int list -> int list -> stored reply -> unit;
  store_partial :
    rpc_id -> Request_id.t -> root -> (int * utxo) list -> stored reply -> unit;
  timer : Timer.t;
}

(* ------------------------------------------------------------------------ *)
(* The component *)

type action =
  | Query of {
      rpc_id : rpc_id;
      anchor_root : root;
      selection_error : string;
      owner : owner;
      required : int;
      rpc_pending : bool;
      serving : peer_id list;
          (** The connected peers serving the anchor root, longest connected first. *)
      spent : Commitment_set.t;  (** The owner's cache entries the mempool spends. *)
    }  (** A send lacks funds: hydrate, with what the root read for it. *)
  | Forget of { rpc_id : rpc_id }
  | Retry_due of { retry : Request_id.t }
  | Cache_stored of { rpc_id : rpc_id; query : Request_id.t; root : root; ok : bool }
  | Partial_stored of { rpc_id : rpc_id; query : Request_id.t; root : root; ok : bool }
  | Wallet_seen of { owner : owner; entries : (int * commitment * int) list }
      (** The world: UTXOs the wallet learns of, and the end of seeding. *)
  | Seeded
  | Started of {
      job : job_id;
      peer : peer_id;
      request_id : Request_id.t;
      request : probe_request;
    }  (** The network's reports, lowered by the root. *)
  | Answered of {
      job : job_id;
      peer : peer_id;
      request_id : Request_id.t;
      answer : peer_answer;
    }
  | Failed of {
      job : job_id;
      peer : peer_id;
      request_id : Request_id.t;
      error : peer_error;
    }
  | Refused of { job : job_id; peer : peer_id }
  | Peer_gone of { peer : peer_id }
  | Verified of {
      job : job_id;
      peer : peer_id;
      request_id : Request_id.t;
      request : probe_request;
      result : (verified, string) result;
    }
[@@deriving message ~name:"ledger.action"]

type effect_ =
  | Verify of {
      job : job_id;
      peer : peer_id;
      request_id : Request_id.t;
      request : probe_request;
      response : probe_response;
    }
  | Store_cache of {
      rpc_id : rpc_id;
      query : Request_id.t;
      root : root;
      found : int list;
      missing : int list;
    }
  | Store_partial of {
      rpc_id : rpc_id;
      query : Request_id.t;
      root : root;
      utxos : (int * utxo) list;
    }
  | Arm_retry of { retry : Request_id.t }
[@@deriving message ~name:"ledger.effect"]

type output =
  | Request of { job : job_id; peer : peer_id; request : probe_request }
      (** Send this probe to the peer. *)
  | Syncing of { rpc_id : rpc_id; query : Request_id.t }
      (** A hydration began for the send. *)
  | Retry of { rpc_id : rpc_id }  (** The hydration ended: try the send again. *)
  | Rejected of { rpc_id : rpc_id; error : string }
      (** The hydration ended: reject the send. *)
[@@deriving message ~name:"ledger.output"]

(* What a reduction emits, gathered while its State changes: a reducer sets
   its State before it emits. *)
type emit = Out of output | Eff of effect_
type ctx = (state, services, action, effect_, output) Ctx.t

let push emits e = Queue.push e emits

(* ------------------------------------------------------------------------ *)
(* The lifecycle *)

(* The job, if it is [Pending] under exactly this peer and request ID. *)
let current state job peer request_id =
  match Job_map.find_opt job state.jobs with
  | Some (j : job) when j.peer = peer && j.phase = Pending { request_id } -> Some j
  | _ -> None

(* Whether the job's consumer still waits for its result. *)
let awaited state job (Spend_hydration { rpc_id }) =
  match Rpc_map.find_opt rpc_id state.hydrations with
  | Some h -> h.stage = Probing { job }
  | None -> false

(* The send's hydration ends unfinished, and its probe with it if it is
   being verified: the network ends a probe still out on it, but nothing
   would end a verification whose answer is lost, and the job would hold
   its peer and a place in flight for good. *)
let forget state rpc_id =
  let state = { state with attempted = Rpc_set.remove rpc_id state.attempted } in
  match Rpc_map.find_opt rpc_id state.hydrations with
  | None -> state
  | Some h -> (
      let state = { state with hydrations = Rpc_map.remove rpc_id state.hydrations } in
      match h.stage with
      | Probing { job } -> (
          match Job_map.find_opt job state.jobs with
          | Some { phase = Verify_pending _; _ } ->
              { state with jobs = Job_map.remove job state.jobs }
          | _ -> state)
      | _ -> state)

let set_hydration (ctx : ctx) rpc_id h =
  Ctx.update ctx (fun s -> { s with hydrations = Rpc_map.add rpc_id h s.hydrations })

let drop_hydration (ctx : ctx) rpc_id =
  Ctx.update ctx (fun s -> { s with hydrations = Rpc_map.remove rpc_id s.hydrations })

(* Waits [retry_cooldown], then has the send tried again. *)
let wait (ctx : ctx) emits rpc_id hydration =
  let retry = Ctx.new_request ctx in
  set_hydration ctx rpc_id { hydration with stage = Waiting { retry } };
  push emits (Eff (Arm_retry { retry }))

(* A hydration's probe ended. *)
let probed (ctx : ctx) emits rpc_id (result : probe_result) =
  match Rpc_map.find_opt rpc_id (Ctx.state ctx).hydrations with
  | Some h when h.stage = Probing { job = result.job } -> (
      match result.result with
      (* Nothing was asked: the probe does not count, and the send waits. *)
      | Error (Rejected _) ->
          Ctx.update ctx (fun s ->
              { s with attempted = Rpc_set.remove rpc_id s.attempted });
          wait ctx emits rpc_id h
      | Error error ->
          drop_hydration ctx rpc_id;
          push emits
            (Out
               (Rejected
                  {
                    rpc_id;
                    error =
                      "hydration probe failed: " ^ Debug.to_string pp_sync_error error;
                  }))
      | Ok verified ->
          let root = verified.root in
          let found_at =
            List.fold_left
              (fun s (index, _) -> Int_set.add index s)
              Int_set.empty verified.found
          in
          let found, missing =
            List.partition
              (fun (item : index_ref) -> Int_set.mem item.index found_at)
              result.request.items
          in
          let awaiting_partial = verified.found <> [] in
          set_hydration ctx rpc_id
            {
              h with
              stage = Storing { awaiting_partial; awaiting_cache = true; found; missing };
            };
          push emits
            (Eff
               (Store_cache
                  {
                    rpc_id;
                    query = h.query;
                    root;
                    found = Int_set.elements found_at;
                    missing = verified.missing;
                  }));
          if awaiting_partial then
            push emits
              (Eff
                 (Store_partial { rpc_id; query = h.query; root; utxos = verified.found }))
      )
  | _ -> ()

(* The one place a result picks its consumer. *)
let route ctx emits (result : probe_result) =
  match result.target with Spend_hydration { rpc_id } -> probed ctx emits rpc_id result

(* Admits a job, or ends it at once with [Rejected]. *)
let start (ctx : ctx) emits job (probe : job) serving =
  let state = Ctx.state ctx in
  let refused =
    if Job_map.mem job state.jobs then Some "duplicate job ID"
    else if Job_map.cardinal state.jobs >= max_in_flight then
      Some "probes in flight at the limit"
    else if has_job state probe.peer then Some "the peer already has a job"
    else if not (List.mem probe.peer serving) then Some "the peer cannot take a request"
    else None
  in
  match refused with
  | Some reason ->
      route ctx emits
        {
          job;
          target = probe.target;
          peer = probe.peer;
          request = probe.request;
          result = Error (Rejected reason);
        }
  | None ->
      push emits (Out (Request { job; peer = probe.peer; request = probe.request }));
      Ctx.update ctx (fun s ->
          { s with jobs = Job_map.add job { probe with phase = Init } s.jobs })

(* Ends a job and routes its result. *)
let finish (ctx : ctx) emits job result =
  match Job_map.find_opt job (Ctx.state ctx).jobs with
  | None -> ()
  | Some (probe : job) ->
      Ctx.update ctx (fun s -> { s with jobs = Job_map.remove job s.jobs });
      route ctx emits
        { job; target = probe.target; peer = probe.peer; request = probe.request; result }

(* ------------------------------------------------------------------------ *)
(* Wallet-spend hydration *)

let plan (ctx : ctx) emits rpc_id anchor_root selection_error owner required rpc_pending
    serving spent =
  let state = Ctx.state ctx in
  if not rpc_pending then Ctx.set_state ctx (forget state rpc_id)
  else
    (* One probe per send at a time: while one is out or being stored, its
       result decides. *)
    let ongoing = Rpc_map.find_opt rpc_id state.hydrations in
    match ongoing with
    | Some { stage = Probing _ | Storing _; _ } -> ()
    | _ -> (
        let items = candidates state owner anchor_root spent required in
        let peer = List.find_opt (fun peer -> not (has_job state peer)) serving in
        let can_probe =
          items <> [] && peer <> None && not (Rpc_set.mem rpc_id state.attempted)
        in
        if (not can_probe) && state.seeded then (
          drop_hydration ctx rpc_id;
          push emits (Out (Rejected { rpc_id; error = selection_error })))
        else
          (* A hydration already waiting goes on under its query. *)
          let query =
            match ongoing with
            | Some h -> h.query
            | None ->
                let query = Ctx.new_request ctx in
                push emits (Out (Syncing { rpc_id; query }));
                query
          in
          let job = Job_id (Ctx.new_request ctx) in
          let hydration = { query; owner; anchor_root; stage = Probing { job } } in
          match peer with
          | Some peer when can_probe ->
              Ctx.update ctx (fun s ->
                  {
                    s with
                    attempted = Rpc_set.add rpc_id s.attempted;
                    hydrations = Rpc_map.add rpc_id hydration s.hydrations;
                  });
              let probe =
                {
                  target = Spend_hydration { rpc_id };
                  peer;
                  request = { root = anchor_root; items };
                  phase = Init;
                }
              in
              start ctx emits job probe serving
          | _ -> wait ctx emits rpc_id hydration)

type store_kind = Cache_store | Partial_store

(* A store acknowledged a write of the send's current hydration (one of an
   earlier hydration, cancelled since, is stale). Its success marks the
   cache entries it makes durable; once both stores have, the send is tried
   again. A failure rejects it. *)
let stored state emits rpc_id query root ok store =
  match Rpc_map.find_opt rpc_id state.hydrations with
  | Some
      ({ stage = Storing { awaiting_partial; awaiting_cache; found; missing }; _ } as h)
    -> (
      let awaiting =
        match store with
        | Cache_store -> awaiting_cache
        | Partial_store -> awaiting_partial
      in
      if h.query <> query || h.anchor_root <> root || not awaiting then state
      else if not ok then (
        let error =
          match store with
          | Cache_store -> "hydration: storing the wallet cache failed"
          | Partial_store -> "hydration: storing the proved UTXOs failed"
        in
        push emits (Out (Rejected { rpc_id; error }));
        { state with hydrations = Rpc_map.remove rpc_id state.hydrations })
      else
        let marks, present, stage =
          match store with
          | Cache_store ->
              ( missing,
                false,
                Storing { awaiting_partial; awaiting_cache = false; found; missing = [] }
              )
          | Partial_store ->
              ( found,
                true,
                Storing { awaiting_partial = false; awaiting_cache; found = []; missing }
              )
        in
        let done_ =
          match stage with
          | Storing { awaiting_partial = false; awaiting_cache = false; _ } -> true
          | _ -> false
        in
        let entries =
          Option.value (Owner_map.find_opt h.owner state.cache) ~default:Int_map.empty
        in
        let entries =
          List.fold_left
            (fun entries (item : index_ref) ->
              match Int_map.find_opt item.index entries with
              | Some entry when entry.commitment = item.expected ->
                  Int_map.add item.index
                    { entry with checked = Some (root, present) }
                    entries
              | _ -> entries)
            entries marks
        in
        let state = { state with cache = Owner_map.add h.owner entries state.cache } in
        match done_ with
        | false ->
            {
              state with
              hydrations = Rpc_map.add rpc_id { h with stage } state.hydrations;
            }
        | true ->
            push emits (Out (Retry { rpc_id }));
            { state with hydrations = Rpc_map.remove rpc_id state.hydrations })
  | _ -> state

let reduce (ctx : ctx) (action : action) _ =
  let emits = Queue.create () in
  (match action with
  | Query
      {
        rpc_id;
        anchor_root;
        selection_error;
        owner;
        required;
        rpc_pending;
        serving;
        spent;
      } ->
      plan ctx emits rpc_id anchor_root selection_error owner required rpc_pending serving
        spent
  | Forget { rpc_id } -> Ctx.set_state ctx (forget (Ctx.state ctx) rpc_id)
  | Retry_due { retry } -> (
      let state = Ctx.state ctx in
      match
        Rpc_map.to_seq state.hydrations
        |> Seq.find (fun (_, h) -> h.stage = Waiting { retry })
      with
      | Some (rpc_id, _) ->
          drop_hydration ctx rpc_id;
          push emits (Out (Retry { rpc_id }))
      | None -> ())
  | Cache_stored { rpc_id; query; root; ok } ->
      Ctx.set_state ctx (stored (Ctx.state ctx) emits rpc_id query root ok Cache_store)
  | Partial_stored { rpc_id; query; root; ok } ->
      Ctx.set_state ctx (stored (Ctx.state ctx) emits rpc_id query root ok Partial_store)
  | Wallet_seen { owner; entries } ->
      (* A leaf can hold another UTXO since: the newer one replaces what the
         cache knew of it. *)
      Ctx.update ctx (fun s ->
          let cache =
            Option.value (Owner_map.find_opt owner s.cache) ~default:Int_map.empty
          in
          let cache =
            List.fold_left
              (fun cache (index, commitment, amount) ->
                match Int_map.find_opt index cache with
                | Some e when e.commitment = commitment -> cache
                | _ -> Int_map.add index { commitment; amount; checked = None } cache)
              cache entries
          in
          { s with cache = Owner_map.add owner cache s.cache })
  | Seeded -> Ctx.update ctx (fun s -> { s with seeded = true })
  | Started { job; peer; request_id; request } -> (
      match Job_map.find_opt job (Ctx.state ctx).jobs with
      | Some pending
        when pending.phase = Init && pending.peer = peer && pending.request = request ->
          Ctx.update ctx (fun s ->
              {
                s with
                jobs =
                  Job_map.add job { pending with phase = Pending { request_id } } s.jobs;
              })
      | _ -> ())
  | Answered { job; peer; request_id; answer } -> (
      match current (Ctx.state ctx) job peer request_id with
      | None -> ()
      | Some pending -> (
          let request = pending.request in
          (* Verification is the one wait nothing else bounds: it is started
             only for a result someone still waits for. *)
          if not (awaited (Ctx.state ctx) job pending.target) then
            finish ctx emits job (Error Abandoned)
          else
            match answer with
            | Unavailable -> finish ctx emits job (Error Unavailable)
            | Data response -> (
                match covers request response with
                | Error error -> finish ctx emits job (Error (Mismatch error))
                | Ok () ->
                    Ctx.update ctx (fun s ->
                        {
                          s with
                          jobs =
                            Job_map.add job
                              { pending with phase = Verify_pending { request_id } }
                              s.jobs;
                        });
                    push emits (Eff (Verify { job; peer; request_id; request; response }))
                )))
  | Failed { job; peer; request_id; error } ->
      if current (Ctx.state ctx) job peer request_id <> None then
        let error : sync_error =
          match error with Timeout -> Timeout | Disconnected -> Disconnected
        in
        finish ctx emits job (Error error)
  | Refused { job; peer } -> (
      match Job_map.find_opt job (Ctx.state ctx).jobs with
      | Some j when j.phase = Init && j.peer = peer ->
          finish ctx emits job (Error (Rejected "the peer cannot take a request"))
      | _ -> ())
  | Peer_gone { peer } ->
      (* The exchange of a job verifying is over: it stays. *)
      let gone =
        Job_map.bindings (Ctx.state ctx).jobs
        |> List.filter (fun (_, (j : job)) ->
               j.peer = peer && match j.phase with Verify_pending _ -> false | _ -> true)
      in
      List.iter (fun (job, _) -> finish ctx emits job (Error Disconnected)) gone
  | Verified { job; peer; request_id; request; result } -> (
      match Job_map.find_opt job (Ctx.state ctx).jobs with
      | None -> ()
      | Some pending ->
          let result =
            if
              pending.phase = Verify_pending { request_id }
              && pending.peer = peer && pending.request = request
            then Result.map_error (fun e -> Bad_proof e) result
            else Error (Mismatch "a verification for another exchange")
          in
          finish ctx emits job result));
  Queue.iter
    (function Out o -> Ctx.emit_output ctx o | Eff e -> Ctx.emit_effect ctx e)
    emits

let handle ctx (eff : effect_) _ =
  let service : services = Effect_ctx.service ctx in
  match eff with
  | Verify { job; peer; request_id; request; response } ->
      let reply = Effect_ctx.reply ctx verify_done_message in
      service.verify job peer request_id request response reply
  | Store_cache { rpc_id; query; root; found; missing } ->
      let reply = Effect_ctx.reply ctx stored_message in
      service.store_cache rpc_id query root found missing reply
  | Store_partial { rpc_id; query; root; utxos } ->
      let reply = Effect_ctx.reply ctx stored_message in
      service.store_partial rpc_id query root utxos reply
  | Arm_retry { retry } ->
      let reply = Effect_ctx.reply ctx Timer.fired in
      Timer.after service.timer ~delay:retry_cooldown reply retry

let component : (state, services, action, effect_, output) Component.t =
  Component.make ~name:"partial_ledger" ~actions:action_message ~effects:effect_message
    ~outputs:output_message ~reduce ~handle
    ~register:(fun r ->
      Registrar.event r verify_done_message (fun (event : verify_done) port ->
          Port.emit_action port
            (Verified
               {
                 job = event.job;
                 peer = event.peer;
                 request_id = event.request_id;
                 request = event.request;
                 result = event.result;
               }));
      Registrar.event r stored_message (fun (event : stored) port ->
          Port.emit_action port
            (match event with
            | Cache { rpc_id; query; root; ok } ->
                Cache_stored { rpc_id; query; root; ok }
            | Partial { rpc_id; query; root; ok } ->
                Partial_stored { rpc_id; query; root; ok }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Retry_due { retry = fired.request })))
    ()
