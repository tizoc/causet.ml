(* The original's tests of the classifier and of chain-placement projection,
   asserting what they asserted. The original fed a whole batch of records
   before outputting; a projection outputs after each record, so the batch
   test counts finalized placements, which are once per block either way. *)

open Chain_placement
open Record
open Classify
module P = Projection

(* ------------------------------------------------------------------------ *)
(* The classifier *)

let produced_block () : produced_block_observation =
  {
    produced_at_ms = 1_000;
    block_hash = "block-hash-41";
    parent_hash = Some "block-hash-40";
    block_height = 41;
    global_slot = 1041;
  }

let tip height timestamp_ms : chain_tip_observation =
  {
    block_hash = Printf.sprintf "block-hash-%d" height;
    height;
    global_slot = height + 1_000;
    timestamp_ms;
  }

let facts () : chain_placement_facts =
  {
    reference_tip = None;
    parent_tip = None;
    settlement_tip = None;
    adoption_tip = None;
    parent_seen_recently = None;
    adoption_sender_count = 0;
    producer_self_adopted = false;
    slot_block_count = 1;
    slot_external_adoption_rank = 0;
    status_updated_at_ms = 1_000;
    as_of_timestamp_ms = 1_100;
  }

let with_reference (f : chain_placement_facts) =
  { f with reference_tip = Some (tip 40 900) }

let with_reference_height height (f : chain_placement_facts) =
  { f with reference_tip = Some (tip height 900) }

let with_parent_seen seen (f : chain_placement_facts) =
  {
    f with
    parent_seen_recently = Some seen;
    parent_tip = (if seen then Some (tip 40 950) else None);
  }

let with_external_adoption senders rank (f : chain_placement_facts) =
  {
    f with
    adoption_sender_count = senders;
    slot_external_adoption_rank = rank;
    adoption_tip = Some (tip 41 1_050);
  }

let with_settlement_height height (f : chain_placement_facts) =
  { f with settlement_tip = Some (tip height 1_100) }

let with_as_of timestamp_ms (f : chain_placement_facts) =
  { f with as_of_timestamp_ms = timestamp_ms }

let str = Alcotest.string
let check_str = Alcotest.check str
let check_bool = Alcotest.(check bool)
let check_opt_str = Alcotest.(check (option string))

let classifies_core_chain_placement_cases () =
  let produced = produced_block () in
  let canonical =
    classify produced
      (facts () |> with_reference |> with_parent_seen true |> with_external_adoption 2 1)
  in
  check_str "canonical" "canonical" canonical.classification;
  check_bool "adopted" true canonical.produced_block_adopted;
  check_bool "not finalized" false canonical.placement_finalized;
  check_opt_str "no reason" None canonical.finalization_reason;

  let finalized_canonical =
    classify produced
      (facts () |> with_reference |> with_parent_seen true |> with_external_adoption 2 1
     |> with_settlement_height 44)
  in
  check_str "finalized canonical" "canonical" finalized_canonical.classification;
  check_bool "finalized" true finalized_canonical.placement_finalized;
  check_opt_str "reason" (Some "chain_progress") finalized_canonical.finalization_reason;

  let orphaned =
    classify produced (facts () |> with_reference |> with_external_adoption 1 2)
  in
  check_str "orphaned" "orphaned" orphaned.classification;
  check_str "orphaned reason" "same_slot_block_has_stronger_external_adoption"
    orphaned.reason;

  let likely_main =
    classify produced (facts () |> with_reference |> with_parent_seen true)
  in
  check_str "likely main" "likely_main_chain" likely_main.classification;
  check_bool "likely main not finalized" false likely_main.placement_finalized;

  let competing_adopted = facts () |> with_reference |> with_parent_seen true in
  let competing_adopted =
    classify produced { competing_adopted with slot_external_adoption_rank = 2 }
  in
  check_str "competing adopted" "orphaned" competing_adopted.classification;
  check_str "competing adopted reason" "same_slot_block_has_stronger_external_adoption"
    competing_adopted.reason;

  let aged_without_progress =
    classify produced
      (facts () |> with_reference |> with_parent_seen true
      |> with_as_of (produced.produced_at_ms + adoption_settle_ms + 1))
  in
  check_str "aged" "likely_main_chain" aged_without_progress.classification;
  check_bool "aged not finalized" false aged_without_progress.placement_finalized;

  let chain_progress_orphaned =
    classify produced
      (facts () |> with_reference |> with_parent_seen true |> with_settlement_height 44)
  in
  check_str "progress orphaned" "orphaned" chain_progress_orphaned.classification;
  check_str "progress orphaned reason" "chain_advanced_without_external_adoption"
    chain_progress_orphaned.reason;
  check_bool "progress finalized" true chain_progress_orphaned.placement_finalized;
  check_opt_str "progress reason" (Some "chain_progress")
    chain_progress_orphaned.finalization_reason;

  let stale_parent =
    classify produced (facts () |> with_reference_height 44 |> with_parent_seen true)
  in
  check_str "stale parent" "stale_parent" stale_parent.classification;

  let missing_reference = classify produced (facts ()) in
  check_str "missing reference" "unknown" missing_reference.classification;
  check_str "missing reference reason" "missing_reference_tip" missing_reference.reason

let builds_a_stable_finalized_key () =
  check_str "key" "chain_placement:produced_block_chain_placement:sender:run:91"
    (finalized_key "sender" "run" 91)

(* ------------------------------------------------------------------------ *)
(* The projection *)

let block hash parent_hash height : chain_block_context_observation =
  {
    block_hash = hash;
    parent_hash = Some parent_hash;
    height;
    global_slot = height + 1_000;
  }

let record_from_run sender run_id event_id timestamp_ms event : Record.t =
  { sender; run_id; event_id; timestamp_ms; event }

let record_from sender event_id timestamp_ms event =
  record_from_run sender "run" event_id timestamp_ms event

let record event_id timestamp_ms event = record_from "sender" event_id timestamp_ms event
let adopted hash parent_hash height = Tip_adopted (block hash parent_hash height)

(* A block injected at the record's time, at slot 1041. *)
let produced timestamp_ms hash parent_hash height =
  Produced
    {
      produced_at_ms = timestamp_ms;
      block_hash = hash;
      parent_hash = Some parent_hash;
      block_height = height;
      global_slot = height + 1_000;
    }

(* Processes [record], appending its outputs to [out]. *)
let process projection record out = out := !out @ P.process projection record

let status_count out =
  List.length (List.filter (function P.Status _ -> true | P.Finalized _ -> false) out)

let finalized_count out =
  List.length (List.filter (function P.Finalized _ -> true | P.Status _ -> false) out)

let check_int = Alcotest.(check int)

let materializes_produced_block_after_chain_progress () =
  let p = P.create () in
  let out = ref [] in
  process p (record 1 1_000 (adopted "parent" "grandparent" 40)) out;
  process p (record 2 1_100 (produced 1_100 "produced" "parent" 41)) out;
  process p (record 3 1_200 (adopted "produced" "parent" 41)) out;
  process p (record 4 1_300 (adopted "settled" "produced" 44)) out;
  check_bool "finalized" true (finalized_count !out > 0)

let late_chain_progress_finalizes_produced_block_outside_adoption_window () =
  let p = P.create () in
  let out = ref [] in
  process p (record 1 1_000 (adopted "parent" "grandparent" 40)) out;
  process p (record 2 1_100 (produced 1_100 "produced" "parent" 41)) out;
  out := [];
  let late = 1_100 + adoption_window_ms + 1 in
  process p (record 3 late (adopted "settled" "other-parent" 44)) out;
  check_bool "finalized" true (finalized_count !out > 0)

let ignores_unrelated_events () =
  let p = P.create () in
  let out = ref [] in
  process p (record 1 1_000 Other) out;
  check_int "nothing" 0 (List.length !out)

let finalized_produced_block_ignores_later_settlement_progress () =
  let p = P.create () in
  let out = ref [] in
  process p (record 1 1_000 (adopted "parent" "grandparent" 40)) out;
  process p (record 2 1_100 (produced 1_100 "produced" "parent" 41)) out;
  out := [];
  process p (record 3 1_300 (adopted "settled" "produced" 44)) out;
  check_int "statuses" 1 (status_count !out);
  check_int "finalized" 1 (finalized_count !out);
  out := [];
  let later = 1_100 + adoption_window_ms + 1 in
  process p (record 4 later (adopted "later-settled" "settled" 45)) out;
  check_int "no status" 0 (status_count !out);
  check_int "no finalized" 0 (finalized_count !out)

let finalized_produced_block_refreshes_late_adoption_in_window_once () =
  let p = P.create () in
  let out = ref [] in
  process p (record_from "producer" 1 1_000 (adopted "parent" "grandparent" 40)) out;
  process p (record_from "producer" 2 1_100 (produced 1_100 "produced" "parent" 41)) out;
  process p (record_from "observer" 3 1_300 (adopted "settled" "produced" 44)) out;
  check_int "finalized" 1 (finalized_count !out);
  out := [];
  process p (record_from "observer" 4 1_200 (adopted "produced" "parent" 41)) out;
  check_int "status" 1 (status_count !out);
  check_int "no finalized" 0 (finalized_count !out)

let same_sender_different_run_adoption_counts_as_external_instance () =
  let p = P.create () in
  let out = ref [] in
  let made = produced 1_100 "produced" "parent" 41 in
  process p (record_from_run "producer" "producer-run" 1 1_100 made) out;
  out := [];
  let seen = adopted "produced" "parent" 41 in
  process p (record_from_run "producer" "observer-run" 2 1_200 seen) out;
  let status =
    List.filter_map
      (function
        | P.Status { status; _ } when status.identity.event_id = 1 -> Some status
        | _ -> None)
      !out
    |> List.rev
    |> function
    | s :: _ -> s
    | [] -> Alcotest.fail "produced block status"
  in
  check_int "senders" 1 status.adoption_sender_count;
  check_bool "not self" false status.producer_self_adopted;
  Alcotest.(check (option int)) "first adopted" (Some 1_200) status.first_adopted_at_ms

let every_block_of_a_batch_is_finalized_once () =
  let p = P.create () in
  let records =
    [ record_from "observer" 1 1_000 (adopted "parent" "grandparent" 40) ]
    @ List.init 5 (fun offset ->
          let height = 41 + offset in
          let made =
            produced (1_100 + offset)
              (Printf.sprintf "produced-%d" height)
              "parent" height
          in
          record_from "producer" (10 + offset) (1_100 + offset) made)
    @ List.init 20 (fun offset ->
          let height = 100 + offset in
          let seen =
            adopted (Printf.sprintf "settled-%d" height) "settled-parent" height
          in
          record_from "observer" (100 + offset) (1_300 + offset) seen)
  in
  let out = ref [] in
  List.iter (fun r -> process p r out) records;
  check_int "finalized" 5 (finalized_count !out)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "chain_placement parity"
    [
      ( "classifier",
        [
          t "classifies core chain placement cases" classifies_core_chain_placement_cases;
          t "builds a stable finalized key" builds_a_stable_finalized_key;
        ] );
      ( "projection",
        [
          t "materializes produced block after chain progress"
            materializes_produced_block_after_chain_progress;
          t "late chain progress finalizes produced block outside adoption window"
            late_chain_progress_finalizes_produced_block_outside_adoption_window;
          t "ignores unrelated events" ignores_unrelated_events;
          t "finalized produced block ignores later settlement progress"
            finalized_produced_block_ignores_later_settlement_progress;
          t "finalized produced block refreshes late adoption in window once"
            finalized_produced_block_refreshes_late_adoption_in_window_once;
          t "same sender different run adoption counts as external instance"
            same_sender_different_run_adoption_counts_as_external_instance;
          t "every block of a batch is finalized once"
            every_block_of_a_batch_is_finalized_once;
        ] );
    ]
