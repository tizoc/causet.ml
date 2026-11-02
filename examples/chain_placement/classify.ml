(* Where a produced block landed, from event-time facts: a pure function,
   with the windows it reads its facts in.

   The classifier assumes trusted reporters, out-of-order arrival, 5-second
   slots and chain reports roughly every 30 seconds. It is an
   eventually-correct observability model, not a consensus proof. Its
   classification and reason strings are its output, and stay stable. *)

open Record

(* Event-time look-behind/look-ahead window for reference and parent
   context. *)
let context_window_ms = 5 * 60 * 1_000

(* Event-time window for external adoption reports of a produced block. *)
let adoption_window_ms = 5 * 60 * 1_000

(* Worst-case tolerated clock skew between senders for event-time
   comparisons. Delayed arrival is handled by the projection, not by
   widening these matches. *)
let clock_skew_tolerance_ms = 2_000

(* How long context is kept, behind the newest event time. *)
let context_retention_ms = 15 * 60 * 1_000

(* Diagnostic age for callers that flag stale observations. Wall-clock age
   never finalizes placement; later chain progress does. *)
let adoption_settle_ms = 2 * 60 * 1_000

(* How many blocks past a produced block the chain must progress before its
   placement is final: a guard against short reorgs. *)
let progress_settle_blocks = 2

(* Height lag, relative to the reference tip, considered suspicious. *)
let height_lag_threshold = 2

(* The processor and output names of a finalized placement. *)
let processor = "chain_placement"
let finalized_output = "produced_block_chain_placement"

(* A chain tip selected from event-time context. *)
type chain_tip_observation = {
  block_hash : string;
  height : int;
  global_slot : int;
  timestamp_ms : int;
}
[@@deriving debug]

(* The event-time facts needed to classify one produced block. *)
type chain_placement_facts = {
  reference_tip : chain_tip_observation option;
  parent_tip : chain_tip_observation option;
  settlement_tip : chain_tip_observation option;
  adoption_tip : chain_tip_observation option;
  parent_seen_recently : bool option;
  adoption_sender_count : int;
  producer_self_adopted : bool;
  slot_block_count : int;
  slot_external_adoption_rank : int;
  status_updated_at_ms : int;
  as_of_timestamp_ms : int;
}

(* One produced block's placement. *)
type chain_placement_result = {
  produced_at_ms : int;
  block_hash : string;
  parent_hash : string option;
  block_height : int;
  global_slot : int;
  reference_tip : chain_tip_observation option;
  parent_tip : chain_tip_observation option;
  settlement_tip : chain_tip_observation option;
  adoption_tip : chain_tip_observation option;
  height_delta : int option;
  chain_progress_blocks : int option;
  parent_seen_recently : bool option;
  adoption_sender_count : int;
  external_adoption_seen : bool;
  producer_self_adopted : bool;
  slot_block_count : int;
  slot_external_adoption_rank : int;
  canonical : bool;
  produced_block_adopted : bool;
  adoption_settled : bool;
  placement_finalized : bool;
  finalization_reason : string option;
  classification : string;
  reason : string;
  updated_at_ms : int;
}
[@@deriving debug]

(* Classifies one produced block from its facts. Missing reference or parent
   evidence gives [unknown] rather than invented context; a block nobody
   else adopted becomes [orphaned] only once chain progress settles it. *)
let classify (produced : produced_block_observation) (facts : chain_placement_facts) =
  let {
    reference_tip;
    parent_tip;
    settlement_tip;
    adoption_tip;
    parent_seen_recently;
    adoption_sender_count;
    producer_self_adopted;
    slot_block_count;
    slot_external_adoption_rank;
    status_updated_at_ms;
    as_of_timestamp_ms = _;
  } =
    facts
  in
  let height_delta =
    Option.map
      (fun (tip : chain_tip_observation) ->
        Saturating.sub produced.block_height tip.height)
      reference_tip
  in
  let external_adoption_seen = adoption_sender_count > 0 in
  let canonical = external_adoption_seen && slot_external_adoption_rank = 1 in
  let produced_block_adopted = external_adoption_seen in
  let chain_progress_blocks =
    Option.map
      (fun (tip : chain_tip_observation) ->
        Saturating.sub tip.height produced.block_height)
      settlement_tip
  in
  let chain_progress_settled =
    match chain_progress_blocks with
    | Some blocks -> blocks >= progress_settle_blocks
    | None -> false
  in
  let placement_finalized = chain_progress_settled in
  let adoption_settled = placement_finalized in
  let finalization_reason =
    if chain_progress_settled then Some "chain_progress" else None
  in
  let max_stale_delta = -height_lag_threshold in
  let stale_height =
    match height_delta with Some delta -> delta <= max_stale_delta | None -> false
  in
  let parent_seen = Option.value parent_seen_recently ~default:false in
  let classification, reason =
    if canonical then ("canonical", "slot_best_external_adoption")
    else if external_adoption_seen then
      ("orphaned", "same_slot_block_has_stronger_external_adoption")
    else if slot_external_adoption_rank > 1 then
      (* Another block of the same slot has external adoption already: this
         one lost its slot, whatever the chain does next. *)
      ("orphaned", "same_slot_block_has_stronger_external_adoption")
    else
      match (reference_tip, produced.parent_hash) with
      | _ when chain_progress_settled ->
          ("orphaned", "chain_advanced_without_external_adoption")
      | None, _ -> ("unknown", "missing_reference_tip")
      | _, None -> ("unknown", "missing_parent_hash")
      | Some _, Some _ when stale_height && not parent_seen ->
          ("likely_side_chain", "height_lag_and_parent_not_recent")
      | Some _, Some _ when stale_height -> ("stale_parent", "height_lag")
      | Some _, Some _ when not parent_seen -> ("stale_parent", "parent_not_recent")
      | Some _, Some _ -> ("likely_main_chain", "parent_seen_recently")
  in
  let updated_at_ms =
    match (adoption_tip, settlement_tip) with
    | Some tip, _ -> tip.timestamp_ms
    | None, Some tip -> max status_updated_at_ms tip.timestamp_ms
    | None, None -> status_updated_at_ms
  in
  {
    produced_at_ms = produced.produced_at_ms;
    block_hash = produced.block_hash;
    parent_hash = produced.parent_hash;
    block_height = produced.block_height;
    global_slot = produced.global_slot;
    reference_tip;
    parent_tip;
    settlement_tip;
    adoption_tip;
    height_delta;
    chain_progress_blocks;
    parent_seen_recently;
    adoption_sender_count;
    external_adoption_seen;
    producer_self_adopted;
    slot_block_count;
    slot_external_adoption_rank;
    canonical;
    produced_block_adopted;
    adoption_settled;
    placement_finalized;
    finalization_reason;
    classification;
    reason;
    updated_at_ms;
  }

(* The stable idempotency key of one block's finalized placement. *)
let finalized_key sender run_id event_id =
  Printf.sprintf "%s:%s:%s:%s:%d" processor finalized_output sender run_id event_id
