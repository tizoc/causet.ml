(* A send waits for each answer at most [Node.answer_timeout]: past it, the
   send gives up and is answered, and a late answer changes nothing. An
   answer in time leaves its deadline to fire stale. The Rust port's
   tests/deadlines.rs. *)

open Causet
open Wallet_send
open Domain
open State
open Common

let timeout = Node.answer_timeout

(* Starts the send and answers its page, so it waits for its proof. *)
let waiting_for_the_proof f =
  ignore (dispatch f (Start { rpc_id }));
  let request = (List.hd f.record.collects).request in
  let c, utxo = wallet_utxo 10 1 in
  let result = page request [ (c, utxo) ] 1 1 true in
  ignore (dispatch f (Persisted_page { request; result }));
  (List.hd f.record.proves).request

let empty_tx = { hash = 1; inputs = []; outputs = [] }

let a_lost_proof_times_out_and_a_late_one_changes_nothing () =
  let f = fixture (state_with_pending_request ()) in
  let prove = waiting_for_the_proof f in
  Clock.Manual.advance f.clock timeout;
  Alcotest.(check int)
    "the page's deadline, stale, and the proof's" 2 (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.check responses "responses"
    [ (rpc_id, rejected "timed out waiting for an answer") ]
    f.record.responses;
  Alcotest.(check bool) "no work" false (has_work (st f).wallet_send rpc_id);
  Alcotest.(check bool)
    "owner free" true
    (owner_is_available (st f).wallet_send second_rpc_id owner);
  Alcotest.(check bool) "no deadlines" true (Pending.is_empty (st f).deadlines);
  let late =
    Node.Proved { request_id = prove.request_id; rpc_id; result = Ok empty_tx }
  in
  Alcotest.check outcome "late" Disabled (dispatch f late);
  Alcotest.(check int) "one response" 1 (List.length f.record.responses)

let an_answer_in_time_leaves_its_deadline_to_fire_stale () =
  let f = fixture (state_with_pending_request ()) in
  let prove = waiting_for_the_proof f in
  ignore
    (dispatch f
       (Node.Proved { request_id = prove.request_id; rpc_id; result = Ok empty_tx }));
  Alcotest.(check int) "queued" 1 (List.length f.record.responses);
  Clock.Manual.advance f.clock timeout;
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.(check int) "no second answer" 1 (List.length f.record.responses);
  Alcotest.(check bool) "queued" true (snd (List.hd f.record.responses)).queued

let a_cancelled_attempt_s_deadline_cannot_end_its_replacement () =
  let f = fixture (state_with_pending_request ()) in
  ignore (dispatch f (Start { rpc_id }));
  Alcotest.(check int) "collects" 1 (List.length f.record.collects);
  (* Halfway through the timeout, the attempt is cancelled and replaced. *)
  Clock.Manual.advance f.clock (timeout / 2);
  ignore (dispatch f (Cancel { rpc_id }));
  ignore (dispatch f (Start { rpc_id }));
  Alcotest.(check int) "collects" 2 (List.length f.record.collects);
  (* The cancelled attempt's deadline passes: the replacement goes on. *)
  Clock.Manual.advance f.clock (timeout / 2);
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.check responses "no response" [] f.record.responses;
  let replacement = (List.nth f.record.collects 1).request in
  Alcotest.(check bool)
    "collecting" true
    (is_collecting_persisted (st f).wallet_send replacement);
  (* Its own deadline still holds. *)
  Clock.Manual.advance f.clock (timeout / 2);
  ignore (Harness.fire_timers f.node);
  deliver_all f;
  Alcotest.(check int) "answered" 1 (List.length f.record.responses)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "wallet send deadlines"
    [
      ( "deadlines",
        [
          t "a lost proof times out and a late one changes nothing"
            a_lost_proof_times_out_and_a_late_one_changes_nothing;
          t "an answer in time leaves its deadline to fire stale"
            an_answer_in_time_leaves_its_deadline_to_fire_stale;
          t "a cancelled attempt's deadline cannot end its replacement"
            a_cancelled_attempt_s_deadline_cannot_end_its_replacement;
        ] );
    ]
