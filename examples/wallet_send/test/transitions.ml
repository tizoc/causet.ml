(* Wallet send's transitions, one scenario per test: paging, stale pins,
   mempool spends and outputs, round-robin mempool scans, cancellation, late
   results and rejections. Where a test needs the world to change (the
   mempool alone moving to a new tip, an RPC dropped unanswered), it
   dispatches a world Action ([Mempool_tip], [Rpc_gone]): a Store has no back
   door into its State. The Rust port's tests/transitions.rs. *)

open Causet
open Wallet_send
open Domain
open State
open Common

let start f rpc_id = dispatch f (Start { rpc_id })
let persisted f (request, result) = dispatch f (Persisted_page { request; result })

let last_prove f =
  match List.rev f.record.asked with
  | Asked_prove request :: _ -> Some request
  | _ -> None

let commitments utxos = List.map fst utxos
let commitment_t = Alcotest.testable pp_commitment ( = )
let check_outcome = Alcotest.check outcome

(* Marshalled and read back: the State saved and restored. *)
let roundtrip (state : Node.state) : Node.state =
  Marshal.from_string (Marshal.to_string state []) 0

let expect_prove f =
  match last_prove f with Some p -> p | None -> Alcotest.fail "proving starts"

let bounded_chunks_preserve_split_funds_across_pages () =
  let f = fixture (state_with_pending_request ()) in
  check_outcome "start" Executed (start f rpc_id);
  let first = collect_request f.record.asked 0 in
  Alcotest.(check (pair int int)) "first page" (0, 0) (first.chunk_seq, first.next_index);
  Alcotest.(check bool) "at root" true (first.anchor_root = root);
  Alcotest.(check bool)
    "collecting" true
    (is_collecting_persisted (st f).wallet_send first);
  let wrong = { first with chunk_seq = 99 } in
  let wrong_page = page wrong [] 0 0 true in
  check_outcome "wrong" Disabled (persisted f (wrong, wrong_page));
  Alcotest.(check int) "asked" 1 (List.length f.record.asked);
  let early_commitment, early = wallet_utxo 4 2 in
  let early_page = page first [ (early_commitment, early) ] 2 1 false in
  check_outcome "early" Executed (persisted f (first, early_page));
  let second = collect_request f.record.asked 1 in
  Alcotest.(check bool) "same request" true (second.request_id = first.request_id);
  Alcotest.(check (pair int int))
    "second page" (1, 1)
    (second.chunk_seq, second.next_index);
  let later_commitment, later = wallet_utxo 6 3 in
  let later_page = page second [ (later_commitment, later) ] 2 2 true in
  check_outcome "later" Executed (persisted f (second, later_page));
  Alcotest.(check int) "asked" 3 (List.length f.record.asked);
  let prove = expect_prove f in
  Alcotest.(check bool) "prove request" true (prove.request_id = first.request_id);
  Alcotest.(check (list commitment_t))
    "inputs"
    [ later_commitment; early_commitment ]
    (commitments prove.available_utxos);
  Alcotest.(check bool)
    "proving" true
    (is_proving (st f).wallet_send rpc_id first.request_id)

let mempool_generation_change_restarts_with_a_fresh_request () =
  let f = fixture (state_with_pending_request ()) in
  check_outcome "start" Executed (start f rpc_id);
  let first = collect_request f.record.asked 0 in
  ignore (dispatch f (Mempool_tip { tip = Block_hash 9L }));
  let empty = page first [] 0 0 true in
  check_outcome "page" Executed (persisted f (first, empty));
  let restarted = collect_request f.record.asked 1 in
  Alcotest.(check bool)
    "a fresh request" true
    (Request_id.compare restarted.request_id first.request_id > 0);
  Alcotest.(check int) "from the start" 0 restarted.chunk_seq;
  Alcotest.(check bool)
    "collecting" true
    (is_collecting_persisted (st f).wallet_send restarted);
  Alcotest.(check bool)
    "owner held" false
    (owner_is_available (st f).wallet_send second_rpc_id owner)

let persisted_spends_are_excluded_and_mempool_outputs_join_selection () =
  let state = state_with_pending_request () in
  let spent_commitment, spent = wallet_utxo 11 20 in
  let retained_commitment, retained = wallet_utxo 20 21 in
  let created_commitment, created = wallet_utxo 12 22 in
  let state =
    { state with mempool = Mempool.accept state.mempool [ spent_commitment ] [ created ] }
  in
  let f = fixture state in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let result =
    page request [ (spent_commitment, spent); (retained_commitment, retained) ] 2 2 true
  in
  check_outcome "page" Executed (persisted f (request, result));
  let prove = expect_prove f in
  Alcotest.(check (list commitment_t))
    "inputs"
    [ retained_commitment; created_commitment ]
    (commitments prove.available_utxos)

let transfer_selection_continues_across_multiple_mempool_pages () =
  let state = state_with_pending_request () in
  let outputs = List.init 64 (fun i -> snd (wallet_utxo 20 (i + 1))) in
  let below c =
    List.length
      (List.filter (fun u -> Commitment_ord.compare (commitment u) c < 0) outputs)
  in
  let winner =
    match
      List.find_opt
        (fun (c, _) -> below c >= mempool_max_scan_positions)
        (List.init 191 (fun i -> wallet_utxo 10 (i + 65)))
    with
    | Some w -> w
    | None -> Alcotest.fail "a winner that sorts beyond the first mempool page"
  in
  let winner_commitment = fst winner in
  let outputs = outputs @ [ snd winner ] in
  let state = { state with mempool = Mempool.accept state.mempool [] outputs } in
  let f = fixture state in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let empty = page request [] 0 0 true in
  check_outcome "page" Executed (persisted f (request, empty));
  Alcotest.(check int) "scans" 1 (List.length f.record.scans);
  Alcotest.(check bool) "not proving" true (last_prove f = None);
  (* The continuation survives a save and a restore of State. *)
  let restored = roundtrip (st f) in
  Alcotest.(check bool)
    "restored" true
    (next_mempool_scan_request restored.wallet_send
    = next_mempool_scan_request (st f).wallet_send);
  ignore (dispatch f Check_timeouts);
  Alcotest.(check int) "scans" 2 (List.length f.record.scans);
  Alcotest.(check bool) "not proving" true (last_prove f = None);
  ignore (dispatch f Check_timeouts);
  Alcotest.(check int) "scans" 3 (List.length f.record.scans);
  let prove = expect_prove f in
  Alcotest.(check int) "inputs" 65 (List.length prove.available_utxos);
  Alcotest.(check bool)
    "the winner" true
    (List.exists (fun (c, _) -> c = winner_commitment) prove.available_utxos);
  ignore (dispatch f Check_timeouts);
  Alcotest.(check int) "scans" 3 (List.length f.record.scans)

let concurrent_mempool_scans_rotate_after_their_immediate_pages () =
  let state = state_with_pending_request () in
  let state =
    {
      state with
      rpc =
        Rpc_map.add second_rpc_id
          { Node.req = request_for second_owner; status = Pending }
          state.rpc;
    }
  in
  let first = List.init 33 (fun i -> snd (wallet_utxo_for owner 20 (i + 1))) in
  let second = List.init 33 (fun i -> snd (wallet_utxo_for second_owner 20 (i + 1))) in
  let mempool = Mempool.accept (Mempool.accept state.mempool [] first) [] second in
  let f = fixture { state with mempool } in
  check_outcome "start" Executed (start f rpc_id);
  let first_request = collect_request f.record.asked 0 in
  check_outcome "page" Executed
    (persisted f (first_request, page first_request [] 0 0 true));
  check_outcome "start" Executed (start f second_rpc_id);
  let second_request = collect_request f.record.asked 1 in
  check_outcome "page" Executed
    (persisted f (second_request, page second_request [] 0 0 true));
  ignore (dispatch f Check_timeouts);
  ignore (dispatch f Check_timeouts);
  Alcotest.(check (list (testable pp_rpc_id ( = ))))
    "scans"
    [ rpc_id; second_rpc_id; rpc_id; second_rpc_id ]
    f.record.scans

let equal_persisted_and_mempool_candidates_keep_the_persisted_input () =
  let state = state_with_pending_request () in
  let persisted_commitment, persisted_utxo = wallet_utxo 10 70 in
  let state =
    { state with mempool = Mempool.accept state.mempool [] [ snd (wallet_utxo 10 71) ] }
  in
  let f = fixture state in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let result = page request [ (persisted_commitment, persisted_utxo) ] 1 1 true in
  check_outcome "page" Executed (persisted f (request, result));
  let prove = expect_prove f in
  Alcotest.(check (option commitment_t))
    "first input" (Some persisted_commitment)
    (Option.map fst (List.nth_opt prove.available_utxos 0))

let cancellation_disables_a_late_chunk_without_continuation () =
  let f = fixture (state_with_pending_request ()) in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  check_outcome "cancel" Executed (dispatch f (Cancel { rpc_id }));
  let late = page request [] 0 0 true in
  check_outcome "late" Disabled (persisted f (request, late));
  Alcotest.(check int) "asked" 1 (List.length f.record.asked);
  Alcotest.(check bool) "no work" false (has_work (st f).wallet_send rpc_id)

let current_collecting_state_roundtrips () =
  let f = fixture (state_with_pending_request ()) in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let restored = roundtrip (st f) in
  Alcotest.(check bool)
    "collecting" true
    (is_collecting_persisted restored.wallet_send request)

let late_prepared_result_clears_correlation_without_dispatching_children () =
  let f = fixture (state_with_pending_request ()) in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let c, utxo = wallet_utxo 10 4 in
  check_outcome "page" Executed
    (persisted f (request, page request [ (c, utxo) ] 1 1 true));
  ignore (dispatch f (Rpc_gone { rpc_id }));
  let asked = List.length f.record.asked and mempool = (st f).mempool in
  let tx = { hash = 1; inputs = []; outputs = [] } in
  let proved = Node.Proved { request_id = request.request_id; rpc_id; result = Ok tx } in
  check_outcome "proved" Executed (dispatch f proved);
  Alcotest.(check bool) "no work" false (has_work (st f).wallet_send rpc_id);
  Alcotest.(check bool) "mempool unchanged" true ((st f).mempool = mempool);
  Alcotest.(check bool) "no RPC" true (Rpc_map.is_empty (st f).rpc);
  Alcotest.(check int) "asked" asked (List.length f.record.asked);
  Alcotest.(check int) "no response" 0 (List.length f.record.responses)

(* A node whose send is already waiting for hydration. *)
let with_hydration_bookkeeping (state : Node.state) : Node.state =
  let id index = { Request_id.session = 1; execution = 1; index } in
  let hydration : Partial_ledger.hydration =
    { query = id 0; owner; anchor_root = root; stage = Waiting { retry = id 1 } }
  in
  {
    state with
    rpc =
      Rpc_map.update rpc_id
        (Option.map (fun (r : Node.rpc_request) -> { r with status = Syncing }))
        state.rpc;
    ledger =
      {
        state.ledger with
        attempted = Rpc_set.add rpc_id state.ledger.attempted;
        hydrations = Rpc_map.add rpc_id hydration state.ledger.hydrations;
      };
  }

let start_stage_rejection_forgets_wallet_spend_hydration () =
  let f = fixture (with_hydration_bookkeeping (state_with_pending_request ())) in
  check_outcome "start" Executed (start f rpc_id);
  let request = collect_request f.record.asked 0 in
  let c, utxo = wallet_utxo 10 80 in
  check_outcome "page" Executed
    (persisted f (request, page request [ (c, utxo) ] 1 1 true));
  Alcotest.(check bool)
    "proving" true
    (is_proving (st f).wallet_send rpc_id request.request_id);
  let rejected =
    Node.Proved
      {
        request_id = request.request_id;
        rpc_id;
        result = Error "prover queue unavailable";
      }
  in
  check_outcome "rejected" Executed (dispatch f rejected);
  Alcotest.(check bool) "forgotten" true (Partial_ledger.forgotten (st f).ledger rpc_id);
  Alcotest.(check bool) "no work" false (has_work (st f).wallet_send rpc_id);
  Alcotest.(check bool) "answered" false (Rpc_map.mem rpc_id (st f).rpc);
  Alcotest.check responses "responses"
    [ (rpc_id, Domain.rejected "prover queue unavailable") ]
    f.record.responses

let start_plan_rejection_forgets_wallet_spend_hydration () =
  let state = with_hydration_bookkeeping (state_with_pending_request ()) in
  let state =
    {
      state with
      rpc =
        Rpc_map.update rpc_id
          (Option.map (fun (r : Node.rpc_request) ->
               { r with req = { r.req with amount = 0 } }))
          state.rpc;
    }
  in
  let f = fixture state in
  check_outcome "start" Executed (start f rpc_id);
  Alcotest.(check bool) "forgotten" true (Partial_ledger.forgotten (st f).ledger rpc_id);
  Alcotest.(check bool) "no work" false (has_work (st f).wallet_send rpc_id);
  Alcotest.(check bool) "answered" false (Rpc_map.mem rpc_id (st f).rpc);
  Alcotest.(check int) "one response" 1 (List.length f.record.responses);
  Alcotest.(check int) "nothing asked" 0 (List.length f.record.asked)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "wallet send transitions"
    [
      ( "transitions",
        [
          t "bounded chunks preserve split funds across pages"
            bounded_chunks_preserve_split_funds_across_pages;
          t "mempool generation change restarts with a fresh request"
            mempool_generation_change_restarts_with_a_fresh_request;
          t "persisted spends are excluded and mempool outputs join selection"
            persisted_spends_are_excluded_and_mempool_outputs_join_selection;
          t "transfer selection continues across multiple mempool pages"
            transfer_selection_continues_across_multiple_mempool_pages;
          t "concurrent mempool scans rotate after their immediate pages"
            concurrent_mempool_scans_rotate_after_their_immediate_pages;
          t "equal persisted and mempool candidates keep the persisted input"
            equal_persisted_and_mempool_candidates_keep_the_persisted_input;
          t "cancellation disables a late chunk without continuation"
            cancellation_disables_a_late_chunk_without_continuation;
          t "current collecting state roundtrips" current_collecting_state_roundtrips;
          t "late prepared result clears correlation without dispatching children"
            late_prepared_result_clears_correlation_without_dispatching_children;
          t "start stage rejection forgets wallet spend hydration"
            start_stage_rejection_forgets_wallet_spend_hydration;
          t "start plan rejection forgets wallet spend hydration"
            start_plan_rejection_forgets_wallet_spend_hydration;
        ] );
    ]
