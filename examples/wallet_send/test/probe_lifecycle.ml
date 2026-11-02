(* Partial-ledger sync's probe lifecycle and wallet-spend hydration, step by
   step: a probe that hydrates a send, the identities that make stale
   reports inert, disconnects, admission, and waiting for the wallet. The
   Rust port's tests/probe_lifecycle.rs. *)

open Causet
open Wallet_send
open Domain
open Ledger
open Common
module L = Partial_ledger
module P = Peers

let setup amounts =
  let f = fixture (state_with_pending_request ()) in
  let ledger = ledger_of amounts in
  move_tip f ledger;
  learn f ledger;
  connect f 1 (Ledger.root ledger);
  (f, ledger)

(* What an honest peer answers. *)
let honest ledger (request : probe_request) =
  let items =
    List.map
      (fun (item : index_ref) ->
        {
          index = item.index;
          utxo = Int_map.find_opt item.index ledger.leaves;
          proof = prove ledger item.index;
        })
      request.items
  in
  Data { root = request.root; items }

let send_ok = function Ok () -> () | Error _ -> Alcotest.fail "the lane is full"

(* Answers the oldest probe sent, and delivers. *)
let answer f answer =
  let send, rest = take_first f.record.sends in
  f.record.sends <- rest;
  let peer, request_id, request = send.request in
  send_ok
    (Reply.try_send_as_worker send.reply { P.peer; request_id; answer = answer request });
  deliver_all f

(* Verifies the oldest response asked, as the verifier would, and
   delivers. *)
let verify f =
  let ask, rest = take_first f.record.verifies in
  f.record.verifies <- rest;
  let v = ask.request in
  send_ok
    (Reply.try_send_as_worker ask.reply
       {
         L.job = v.job;
         peer = v.peer;
         request_id = v.request_id;
         request = v.probe;
         result = verify_response v.probe v.response;
       });
  deliver_all f

let ack_cache f ok =
  let store, rest = take_first f.record.cache_stores in
  f.record.cache_stores <- rest;
  let rpc_id, query, root, _, _ = store.request in
  send_ok (Reply.try_send_as_worker store.reply (L.Cache { rpc_id; query; root; ok }));
  deliver_all f

let ack_partial f ok =
  let store, rest = take_first f.record.partial_stores in
  f.record.partial_stores <- rest;
  let rpc_id, query, root, _ = store.request in
  send_ok (Reply.try_send_as_worker store.reply (L.Partial { rpc_id; query; root; ok }));
  deliver_all f

let stage f =
  Option.map
    (fun (h : L.hydration) -> h.stage)
    (Rpc_map.find_opt rpc_id (st f).ledger.hydrations)

let is_storing f = match stage f with Some (Storing _) -> true | _ -> false
let is_waiting f = match stage f with Some (Waiting _) -> true | _ -> false
let first_error f = error_of (snd (List.hd f.record.responses))

let a_probe_hydrates_the_send_which_is_tried_again_once_both_stores_are_done () =
  List.iter
    (fun cache_first ->
      let f, ledger = setup [ 30; 20 ] in
      reach_hydration f;
      (* The newest leaf covers the amount. *)
      let peer, _, asked = (List.hd f.record.sends).request in
      Alcotest.(check bool) "peer 1" true (peer = Peer_id 1);
      Alcotest.(check (list int))
        "leaf 1" [ 1 ]
        (List.map (fun (i : index_ref) -> i.index) asked.items);
      answer f (honest ledger);
      verify f;
      Alcotest.(check bool) "storing" true (is_storing f);
      let _, _, _, stored = (List.hd f.record.partial_stores).request in
      Alcotest.(check bool)
        "stored leaf 1" true
        (stored = [ (1, Int_map.find 1 ledger.leaves) ]);
      (* Not before both stores are done, in either order. *)
      if cache_first then ack_cache f true else ack_partial f true;
      Alcotest.(check int) "not yet" 1 (List.length f.record.collects);
      if cache_first then ack_partial f true else ack_cache f true;
      Alcotest.(check int) "tried again" 2 (List.length f.record.collects);
      Alcotest.(check bool)
        "no hydration deadline" false
        (List.exists
           (fun (a : Node.awaited) ->
             match a.answer with Hydration _ -> true | _ -> false)
           (Pending.values (st f).deadlines));
      (* One probe per send: still short of funds, it is rejected. *)
      let request = (List.nth f.record.collects 1).request in
      let result = page request [] 0 0 true in
      ignore (dispatch f (Persisted_page { request; result }));
      Alcotest.(check int) "no probe" 0 (List.length f.record.sends);
      Alcotest.(check int) "one response" 1 (List.length f.record.responses);
      Alcotest.(check bool) "insufficient" true (contains (first_error f) "insufficient"))
    [ false; true ]

let a_store_failure_rejects_the_send () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (honest ledger);
  verify f;
  ack_cache f false;
  Alcotest.check responses "responses"
    [ (rpc_id, rejected "hydration: storing the wallet cache failed") ]
    f.record.responses;
  (* The other store's acknowledgement, late, changes nothing. *)
  ack_partial f true;
  Alcotest.(check int) "collects" 1 (List.length f.record.collects)

let a_forgotten_hydration_s_probe_cannot_finish_its_replacement () =
  let f, ledger = setup [ 30 ] in
  connect f 2 (Ledger.root ledger);
  reach_hydration f;
  ignore (dispatch f (Cancel { rpc_id }));
  Alcotest.(check bool) "forgotten" true (L.forgotten (st f).ledger rpc_id);
  (* The replacement probes the other peer: the first still has its job. *)
  reach_hydration f;
  let peer, _, _ = (List.nth f.record.sends 1).request in
  Alcotest.(check bool) "peer 2" true (peer = Peer_id 2);
  let job =
    match stage f with Some (Probing { job }) -> job | _ -> Alcotest.fail "probing"
  in
  (* The first probe fails, late: its result reaches no hydration. *)
  answer f (fun _ -> Unavailable);
  Alcotest.check responses "no response" [] f.record.responses;
  Alcotest.(check bool) "still probing" true (stage f = Some (Probing { job }));
  Alcotest.(check int) "one job" 1 (L.Job_map.cardinal (st f).ledger.jobs)

let another_start_waits_for_the_probe () =
  let f, _ = setup [ 30 ] in
  connect f 2 (st f).chain_root;
  reach_hydration f;
  Alcotest.check outcome "start" Disabled (dispatch f (Start { rpc_id }));
  Alcotest.(check int) "collects" 1 (List.length f.record.collects);
  Alcotest.(check int) "one probe out at a time" 1 (List.length f.record.sends)

let a_probe_times_out_and_its_late_answer_changes_nothing () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  Clock.Manual.advance f.clock P.request_timeout;
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.check responses "responses"
    [ (rpc_id, rejected "hydration probe failed: Timeout") ]
    f.record.responses;
  Alcotest.(check bool)
    "none outstanding" true
    (Request_map.is_empty (st f).peers.outstanding);
  answer f (honest ledger);
  Alcotest.(check int) "dropped by the network layer" 0 (List.length f.record.verifies);
  Alcotest.(check int) "one response" 1 (List.length f.record.responses)

let a_disconnect_fails_a_pending_probe () =
  let f, _ = setup [ 30 ] in
  reach_hydration f;
  ignore (Harness.dispatch f.node f.peers (Disconnected { peer = Peer_id 1 }));
  Alcotest.check responses "responses"
    [ (rpc_id, rejected "hydration probe failed: Disconnected") ]
    f.record.responses;
  Alcotest.(check bool) "no jobs" true (L.Job_map.is_empty (st f).ledger.jobs)

let a_disconnect_keeps_a_probe_being_verified () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (honest ledger);
  send_ok
    (Sink.try_send_to_as_worker (Harness.sink f.node) f.peers P.link_event_message
       (Disconnected { peer = Peer_id 1 }));
  deliver_all f;
  Alcotest.(check int) "kept" 1 (L.Job_map.cardinal (st f).ledger.jobs);
  verify f;
  Alcotest.(check bool) "no jobs" true (L.Job_map.is_empty (st f).ledger.jobs);
  Alcotest.(check bool) "storing" true (is_storing f);
  Alcotest.check responses "no response" [] f.record.responses

let a_verification_for_another_exchange_ends_the_job () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (honest ledger);
  let ask, rest = take_first f.record.verifies in
  f.record.verifies <- rest;
  let v = ask.request in
  send_ok
    (Reply.try_send_as_worker ask.reply
       {
         L.job = v.job;
         peer = v.peer;
         request_id = { v.request_id with index = v.request_id.index + 100 };
         request = v.probe;
         result = verify_response v.probe v.response;
       });
  deliver_all f;
  Alcotest.(check bool) "no jobs" true (L.Job_map.is_empty (st f).ledger.jobs);
  Alcotest.(check bool) "Mismatch" true (contains (first_error f) "Mismatch");
  Alcotest.(check int) "nothing stored" 0 (List.length f.record.cache_stores)

let an_answer_that_does_not_cover_the_request_is_not_verified () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (fun request ->
      match honest ledger request with
      | Data response ->
          Data { response with items = response.items @ [ List.hd response.items ] }
      | Unavailable -> assert false);
  Alcotest.(check int) "not verified" 0 (List.length f.record.verifies);
  Alcotest.(check bool) "twice" true (contains (first_error f) "twice")

let a_bad_proof_rejects_the_send_and_stores_nothing () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (fun request ->
      match honest ledger request with
      | Data ({ items = first :: rest; _ } as response) ->
          let utxo = Option.get first.utxo in
          Data
            {
              response with
              items =
                { first with utxo = Some { utxo with amount = utxo.amount + 1_000 } }
                :: rest;
            }
      | _ -> assert false);
  verify f;
  Alcotest.(check bool) "BadProof" true (contains (first_error f) "BadProof");
  Alcotest.(check int) "nothing stored" 0 (List.length f.record.partial_stores);
  Alcotest.(check int) "nothing checked" 0 (List.length (L.checked (st f).ledger))

let a_probe_over_capacity_waits_and_the_send_is_tried_again () =
  let state = state_with_pending_request () in
  let ledger = ledger_of [ 30 ] in
  (* Four probes already in flight, to other peers. *)
  let jobs =
    List.fold_left
      (fun jobs n ->
        let job = L.Job_id { Request_id.session = 9; execution = 9; index = n } in
        let probe : L.job =
          {
            target = Spend_hydration { rpc_id = Rpc_id (100 + n) };
            peer = Peer_id (10 + n);
            request = { root = Ledger.root ledger; items = [] };
            phase = Init;
          }
        in
        L.Job_map.add job probe jobs)
      state.ledger.jobs [ 0; 1; 2; 3 ]
  in
  let f = fixture { state with ledger = { state.ledger with jobs } } in
  move_tip f ledger;
  learn f ledger;
  connect f 1 (Ledger.root ledger);
  reach_hydration f;
  Alcotest.(check int) "no probe" 0 (List.length f.record.sends);
  Alcotest.(check bool) "waiting" true (is_waiting f);
  Alcotest.(check bool) "not counted" false (Rpc_set.mem rpc_id (st f).ledger.attempted);
  Clock.Manual.advance f.clock L.retry_cooldown;
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.(check int) "tried again" 2 (List.length f.record.collects);
  Alcotest.check responses "no response" [] f.record.responses

let an_unseeded_wallet_waits_and_a_seeded_one_rejects () =
  let f = fixture (state_with_pending_request ()) in
  reach_hydration f;
  Alcotest.(check bool) "waiting" true (is_waiting f);
  Alcotest.check responses "no response" [] f.record.responses;
  Clock.Manual.advance f.clock L.retry_cooldown;
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.(check int) "tried again" 2 (List.length f.record.collects);
  ignore (Harness.dispatch f.node f.ledger Seeded);
  let request = (List.nth f.record.collects 1).request in
  let result = page request [] 0 0 true in
  ignore (dispatch f (Persisted_page { request; result }));
  Alcotest.(check bool) "insufficient" true (contains (first_error f) "insufficient");
  Alcotest.(check bool) "forgotten" true (L.forgotten (st f).ledger rpc_id);
  Alcotest.(check bool) "no deadlines" true (Pending.is_empty (st f).deadlines)

(* Regression: a store acknowledgement names its hydration. One of a
   hydration cancelled since cannot end the one that replaced it, at the
   same RPC and root. *)
let an_old_store_acknowledgement_cannot_end_the_replacement () =
  let f, ledger = setup [ 30; 20 ] in
  reach_hydration f;
  answer f (honest ledger);
  verify f;
  let obsolete, rest = take_first f.record.cache_stores in
  f.record.cache_stores <- rest;
  let first_query = Option.get (L.hydration_query (st f).ledger rpc_id) in
  ignore (dispatch f (Cancel { rpc_id }));
  reach_hydration f;
  answer f (honest ledger);
  verify f;
  let current_query = Option.get (L.hydration_query (st f).ledger rpc_id) in
  Alcotest.(check bool) "another query" true (first_query <> current_query);
  Alcotest.(check bool) "storing" true (is_storing f);
  let rpc, query, root, _, _ = obsolete.request in
  send_ok
    (Reply.try_send_as_worker obsolete.reply
       (L.Cache { rpc_id = rpc; query; root; ok = false }));
  deliver_all f;
  Alcotest.check responses "no response" [] f.record.responses;
  Alcotest.(check bool)
    "the replacement" true
    (L.hydration_query (st f).ledger rpc = Some current_query)

(* Regression: a verification whose answer is lost does not hold its peer
   once the send has ended. *)
let a_lost_verification_does_not_hold_its_peer_after_the_send_ends () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (honest ledger);
  (match L.Job_map.bindings (st f).ledger.jobs with
  | [ (_, { phase = Verify_pending _; _ }) ] -> ()
  | _ -> Alcotest.fail "verifying");
  (* The verifier never answers; the send times out. *)
  Clock.Manual.advance f.clock Node.answer_timeout;
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.(check int) "one response" 1 (List.length f.record.responses);
  Alcotest.(check bool) "no hydrations" true (Rpc_map.is_empty (st f).ledger.hydrations);
  Alcotest.(check bool) "no jobs" true (L.Job_map.is_empty (st f).ledger.jobs)

(* Regression: a leaf found is not held as checked until the partial store
   has it, so a failed write leaves it for a later send to probe. *)
let a_failed_partial_write_can_be_probed_again () =
  let f, ledger = setup [ 30 ] in
  reach_hydration f;
  answer f (honest ledger);
  verify f;
  ack_cache f true;
  ack_partial f false;
  Alcotest.(check int) "one response" 1 (List.length f.record.responses);
  Alcotest.(check int) "nothing checked" 0 (List.length (L.checked (st f).ledger));
  ignore (dispatch f (Send { rpc_id = second_rpc_id; req = request }));
  let next = (last f.record.collects).request in
  let result = page next [] 0 0 true in
  ignore (dispatch f (Persisted_page { request = next; result }));
  Alcotest.(check int) "one response" 1 (List.length f.record.responses);
  Alcotest.(check int) "probed again at the same root" 1 (List.length f.record.sends)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "wallet send probe lifecycle"
    [
      ( "probe lifecycle",
        [
          t "a probe hydrates the send, which is tried again once both stores are done"
            a_probe_hydrates_the_send_which_is_tried_again_once_both_stores_are_done;
          t "a store failure rejects the send" a_store_failure_rejects_the_send;
          t "a forgotten hydration's probe cannot finish its replacement"
            a_forgotten_hydration_s_probe_cannot_finish_its_replacement;
          t "another start waits for the probe" another_start_waits_for_the_probe;
          t "a probe times out and its late answer changes nothing"
            a_probe_times_out_and_its_late_answer_changes_nothing;
          t "a disconnect fails a pending probe" a_disconnect_fails_a_pending_probe;
          t "a disconnect keeps a probe being verified"
            a_disconnect_keeps_a_probe_being_verified;
          t "a verification for another exchange ends the job"
            a_verification_for_another_exchange_ends_the_job;
          t "an answer that does not cover the request is not verified"
            an_answer_that_does_not_cover_the_request_is_not_verified;
          t "a bad proof rejects the send and stores nothing"
            a_bad_proof_rejects_the_send_and_stores_nothing;
          t "a probe over capacity waits and the send is tried again"
            a_probe_over_capacity_waits_and_the_send_is_tried_again;
          t "an unseeded wallet waits and a seeded one rejects"
            an_unseeded_wallet_waits_and_a_seeded_one_rejects;
          t "an old store acknowledgement cannot end the replacement"
            an_old_store_acknowledgement_cannot_end_the_replacement;
          t "a lost verification does not hold its peer after the send ends"
            a_lost_verification_does_not_hold_its_peer_after_the_send_ends;
          t "a failed partial write can be probed again"
            a_failed_partial_write_can_be_probed_again;
        ] );
    ]
