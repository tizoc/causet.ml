(* Chain placement as a projection: it keeps a bounded context of the chain
   reports it has seen and the blocks produced recently, and after each
   record recomputes the blocks the record may have changed ("dirty"),
   outputting a block's status when it changes and its placement once when
   it becomes final. Blocks are kept [Classify.context_retention_ms] behind
   the newest event time seen; past that, a block stays a while as a
   competitor for the blocks of its slot still kept, and chain reports stay
   as long as a kept block or a competitor reads them. Facts are compared by
   event time; records arrive in log order.

   Its State is a mutable record, [t]; [projection] gives the projector its
   view of it. Collections are ordered maps and sets, as the Rust port's
   BTreeMap and BTreeSet, so what it outputs never depends on hashing. *)

open Record
open Classify

module Ids = Map.Make (struct
  type t = event_identity

  let compare = compare
end)

module Id_set = Set.Make (struct
  type t = event_identity

  let compare = compare
end)

module Keys = Map.Make (String)
module Strings = Set.Make (String)

module Pairs = Set.Make (struct
  type t = string * string

  let compare = compare
end)

module By_sequence = Map.Make (Int)

(* A produced block's status: its facts as of now. *)
type status = {
  identity : event_identity;
  produced : produced_block_observation;
  reference_tip : chain_tip_observation option;
  parent_seen_recently : bool option;
  parent_seen_at_ms : int option;
  adoption_sender_count : int;
  producer_self_adopted : bool;
  first_adopted_at_ms : int option;
  last_adopted_at_ms : int option;
  settlement_tip : chain_tip_observation option;
  slot_block_count : int;
  slot_external_adoption_rank : int;
  updated_at_ms : int;
}
[@@deriving debug]

(* A produced block's placement, once final. *)
type finalized = {
  identity : event_identity;
  produced_at_ms : int;
  chain_placement : chain_placement_result;
}
[@@deriving debug]

(* What the projection outputs, each keyed as the application stores it: a
   status by its block, a finalized placement by its idempotency key.
   [pp_output] prints it in the Rust port's [Debug] format, so that the two
   ports' outputs and failure messages can be compared as text. *)
type output =
  | Status of { key : string; status : status }
  | Finalized of { key : string; finalized : finalized }
[@@deriving debug]

(* An output's key. *)
let key = function Status { key; _ } | Finalized { key; _ } -> key

let status_key (identity : event_identity) =
  Printf.sprintf "status:%s:%s:%d" identity.sender identity.run_id identity.event_id

let status_output (status : status) = Status { key = status_key status.identity; status }

let finalized_output (finalized : finalized) =
  let id = finalized.identity in
  Finalized { key = finalized_key id.sender id.run_id id.event_id; finalized }

type context_entry = {
  sequence : int;  (** Arrival order among the context entries, for ties. *)
  sender : string;
  run_id : string;
  timestamp_ms : int;
  observation : chain_block_context_observation;
}

type produced_block_state = {
  produced : produced_block_observation;
  last_status : status option;
  placement_finalized : bool;
  finalized_output_emitted : bool;
}

type adoption_summary = {
  sender_count : int;
  first_seen_at_ms : int option;
  last_seen_at_ms : int option;
  producer_self_adopted : bool;
}

(* A production as the projection reads it: produced no later than the
   clock skew tolerance after its report's own time. A report timed further
   before the production it claims would be read by its production time but
   replayed by a warm-up by its own: kept, as a block or a competitor, past
   where a restart can find it, so an eviction would change a kept block's
   status only where there was no restart. A producer times its report of a
   production at the production itself (its later updates are other
   events), so the original never sees such a report: this changes nothing
   it reads, and keeps a projection fed one deterministic across restarts. *)
let as_read (produced : produced_block_observation) record_ms =
  let latest = Saturating.add record_ms clock_skew_tolerance_ms in
  { produced with produced_at_ms = min produced.produced_at_ms latest }

(* How much longer than kept blocks the context they read is kept: a kept
   block's competitors were produced up to [adoption_window_ms] before it,
   and a competitor's adoptions are read from [clock_skew_tolerance_ms]
   before it; a kept block's own reference and parent, [context_window_ms]
   before it. *)
let context_reach_ms =
  if context_window_ms > adoption_window_ms + clock_skew_tolerance_ms then
    context_window_ms
  else adoption_window_ms + clock_skew_tolerance_ms

(* The chain-placement projection. *)
type t = {
  mutable contexts : context_entry By_sequence.t;  (** In arrival order. *)
  mutable produced_blocks : produced_block_state Ids.t;
  mutable status_keys : event_identity Keys.t;
      (** Each kept block, by the key of its status output. *)
  mutable competitors : produced_block_observation Ids.t;
      (** Blocks no longer kept, still read as same-slot competitors by the blocks that
          are. *)
  mutable next_sequence : int;
  mutable max_timestamp_ms : int option;
  retention_ms : int option;
      (** How far behind the newest event time context is kept; [None] keeps everything.
      *)
}

let make retention_ms =
  {
    contexts = By_sequence.empty;
    produced_blocks = Ids.empty;
    status_keys = Keys.empty;
    competitors = Ids.empty;
    next_sequence = 0;
    max_timestamp_ms = None;
    retention_ms;
  }

(* The projection, keeping context [context_retention_ms] behind the newest
   event time. *)
let create () = make (Some context_retention_ms)

(* The same projection keeping everything it sees: the definition the
   bounded one approximates, for tests to compare it with. *)
let unbounded () = make None
let name = processor

(* How far back a warm-up must reach: as far as context is kept. *)
let retention_ms t = Option.map (fun ms -> ms + context_reach_ms) t.retention_ms

(* ------------------------------------------------------------------------ *)
(* Facts *)

let same_slot_candidate (left : produced_block_observation)
    (right : produced_block_observation) =
  left.global_slot = right.global_slot
  && left.block_height = right.block_height
  && (left.parent_hash = None || right.parent_hash = None
     || left.parent_hash = right.parent_hash)
  && abs (left.produced_at_ms - right.produced_at_ms) <= adoption_window_ms

let chain_tip (entry : context_entry) =
  {
    block_hash = entry.observation.block_hash;
    height = entry.observation.height;
    global_slot = entry.observation.global_slot;
    timestamp_ms = entry.timestamp_ms;
  }

(* The entry with the greatest key; of equal keys, the last (Rust's
   [max_by_key]). *)
let max_by key contexts =
  By_sequence.fold
    (fun _ entry best ->
      match best with
      | Some b when compare (key entry) (key b) < 0 -> best
      | _ -> Some entry)
    contexts None

(* The entry with the smallest key; of equal keys, the first (Rust's
   [min_by_key]). *)
let min_by key contexts =
  By_sequence.fold
    (fun _ entry best ->
      match best with
      | Some b when compare (key entry) (key b) >= 0 -> best
      | _ -> Some entry)
    contexts None

(* The highest report in the window around the block. *)
let reference_tip t (produced : produced_block_observation) =
  let from_ms = Saturating.sub produced.produced_at_ms context_window_ms in
  let to_ms = Saturating.add produced.produced_at_ms clock_skew_tolerance_ms in
  By_sequence.filter
    (fun _ entry -> entry.timestamp_ms >= from_ms && entry.timestamp_ms <= to_ms)
    t.contexts
  |> max_by (fun e -> (e.observation.height, e.timestamp_ms, e.sequence))
  |> Option.map chain_tip

(* The latest report of the block's parent in the window. *)
let parent_tip t (produced : produced_block_observation) =
  match produced.parent_hash with
  | None -> None
  | Some parent_hash ->
      let from_ms = Saturating.sub produced.produced_at_ms context_window_ms in
      let to_ms = Saturating.add produced.produced_at_ms clock_skew_tolerance_ms in
      By_sequence.filter
        (fun _ entry ->
          entry.observation.block_hash = parent_hash
          && entry.timestamp_ms >= from_ms && entry.timestamp_ms <= to_ms)
        t.contexts
      |> max_by (fun e -> (e.timestamp_ms, e.sequence))
      |> Option.map chain_tip

(* The first report at least [progress_settle_blocks] above the block. *)
let settlement_tip t (produced : produced_block_observation) =
  let settle_height = Saturating.add produced.block_height progress_settle_blocks in
  By_sequence.filter
    (fun _ entry ->
      entry.observation.height >= settle_height
      && entry.timestamp_ms >= produced.produced_at_ms)
    t.contexts
  |> min_by (fun e -> (e.timestamp_ms, e.sequence))
  |> Option.map chain_tip

(* Who else adopted the block within the adoption window: each other sender
   and run once. The producer's own run does not count. *)
let adoption_summary t (producer : event_identity) (produced : produced_block_observation)
    =
  let from_ms = Saturating.sub produced.produced_at_ms clock_skew_tolerance_ms in
  let to_ms = Saturating.add produced.produced_at_ms adoption_window_ms in
  let externals, summary =
    By_sequence.fold
      (fun _ entry (externals, summary) ->
        if
          entry.observation.block_hash = produced.block_hash
          && entry.timestamp_ms >= from_ms && entry.timestamp_ms <= to_ms
        then
          if entry.sender = producer.sender && entry.run_id = producer.run_id then
            (externals, { summary with producer_self_adopted = true })
          else
            let at = entry.timestamp_ms in
            ( Pairs.add (entry.sender, entry.run_id) externals,
              {
                summary with
                first_seen_at_ms =
                  Some
                    (match summary.first_seen_at_ms with
                    | Some f -> min f at
                    | None -> at);
                last_seen_at_ms =
                  Some
                    (match summary.last_seen_at_ms with Some l -> max l at | None -> at);
              } )
        else (externals, summary))
      t.contexts
      ( Pairs.empty,
        {
          sender_count = 0;
          first_seen_at_ms = None;
          last_seen_at_ms = None;
          producer_self_adopted = false;
        } )
  in
  { summary with sender_count = Pairs.cardinal externals }

type adoption_rank = {
  rank_sender_count : int;
  rank_last_seen_at_ms : int;
  rank_produced_at_ms : int;
  rank_block_hash : string;
}

(* More external senders first, then the latest adoption, then the earliest
   produced, then the smaller hash. *)
let is_better_than a b =
  a.rank_sender_count > b.rank_sender_count
  || a.rank_sender_count = b.rank_sender_count
     && a.rank_last_seen_at_ms > b.rank_last_seen_at_ms
  || a.rank_sender_count = b.rank_sender_count
     && a.rank_last_seen_at_ms = b.rank_last_seen_at_ms
     && a.rank_produced_at_ms < b.rank_produced_at_ms
  || a.rank_sender_count = b.rank_sender_count
     && a.rank_last_seen_at_ms = b.rank_last_seen_at_ms
     && a.rank_produced_at_ms = b.rank_produced_at_ms
     && a.rank_block_hash < b.rank_block_hash

let rank (adoption : adoption_summary) (produced : produced_block_observation) =
  {
    rank_sender_count = adoption.sender_count;
    rank_last_seen_at_ms = Option.value adoption.last_seen_at_ms ~default:0;
    rank_produced_at_ms = produced.produced_at_ms;
    rank_block_hash = produced.block_hash;
  }

(* How many blocks compete for the slot, and this one's rank among those with
   external adoption (1 is best). *)
let same_slot_competition t (produced : produced_block_observation)
    (adoption : adoption_summary) =
  let current_rank = rank adoption produced in
  let candidates =
    Ids.fold
      (fun identity state acc -> (identity, state.produced) :: acc)
      t.produced_blocks []
    |> List.rev
  in
  let candidates = candidates @ Ids.bindings t.competitors in
  let same_slot, better =
    List.fold_left
      (fun (same_slot, better) (other_identity, (other : produced_block_observation)) ->
        if not (same_slot_candidate produced other) then (same_slot, better)
        else
          let same_slot = Strings.add other.block_hash same_slot in
          if other.block_hash = produced.block_hash then (same_slot, better)
          else
            let other_adoption = adoption_summary t other_identity other in
            if other_adoption.sender_count = 0 then (same_slot, better)
            else if is_better_than (rank other_adoption other) current_rank then
              (same_slot, Strings.add other.block_hash better)
            else (same_slot, better))
      (Strings.empty, Strings.empty)
      candidates
  in
  (Strings.cardinal same_slot, Strings.cardinal better + 1)

let status t identity (produced : produced_block_observation) =
  let reference_tip = reference_tip t produced in
  let parent_tip = parent_tip t produced in
  let settlement_tip = settlement_tip t produced in
  let adoption = adoption_summary t identity produced in
  let parent_seen_recently =
    Option.map (fun _ -> Option.is_some parent_tip) produced.parent_hash
  in
  let tip_time = Option.map (fun (tip : chain_tip_observation) -> tip.timestamp_ms) in
  let parent_seen_at_ms = tip_time parent_tip in
  let slot_block_count, slot_external_adoption_rank =
    same_slot_competition t produced adoption
  in
  let updated_at_ms =
    List.fold_left max produced.produced_at_ms
      (List.filter_map Fun.id
         [
           tip_time reference_tip;
           parent_seen_at_ms;
           adoption.first_seen_at_ms;
           adoption.last_seen_at_ms;
           tip_time settlement_tip;
         ])
  in
  {
    identity;
    produced;
    reference_tip;
    parent_seen_recently;
    parent_seen_at_ms;
    adoption_sender_count = adoption.sender_count;
    producer_self_adopted = adoption.producer_self_adopted;
    first_adopted_at_ms = adoption.first_seen_at_ms;
    last_adopted_at_ms = adoption.last_seen_at_ms;
    settlement_tip;
    slot_block_count;
    slot_external_adoption_rank;
    updated_at_ms;
  }

let chain_placement t (status : status) =
  let produced = status.produced in
  let adoption_tip =
    Option.map
      (fun timestamp_ms ->
        {
          block_hash = produced.block_hash;
          height = produced.block_height;
          global_slot = produced.global_slot;
          timestamp_ms;
        })
      (match status.first_adopted_at_ms with
      | Some _ as first -> first
      | None -> status.last_adopted_at_ms)
  in
  let parent_tip =
    Option.map
      (fun timestamp_ms ->
        {
          block_hash = Option.value produced.parent_hash ~default:"";
          height = Saturating.sub produced.block_height 1;
          global_slot = Saturating.sub produced.global_slot 1;
          timestamp_ms;
        })
      status.parent_seen_at_ms
  in
  classify produced
    {
      reference_tip = status.reference_tip;
      parent_tip;
      settlement_tip = status.settlement_tip;
      adoption_tip;
      parent_seen_recently = status.parent_seen_recently;
      adoption_sender_count = status.adoption_sender_count;
      producer_self_adopted = status.producer_self_adopted;
      slot_block_count = status.slot_block_count;
      slot_external_adoption_rank = status.slot_external_adoption_rank;
      status_updated_at_ms = status.updated_at_ms;
      as_of_timestamp_ms = Option.value t.max_timestamp_ms ~default:status.updated_at_ms;
    }

(* The status output [key] names, recomputed now from the context kept,
   whether or not anything marked its block; [None] when the key names no
   block kept (one evicted, or a finalized placement's key). What a check of
   incremental against from-scratch asks ([check_incremental]). *)
let status_now t key =
  match Keys.find_opt key t.status_keys with
  | None -> None
  | Some identity -> (
      match Ids.find_opt identity t.produced_blocks with
      | None -> None
      | Some state -> Some (status_output (status t identity state.produced)))

(* ------------------------------------------------------------------------ *)
(* Processing *)

let observe_timestamp t timestamp_ms =
  t.max_timestamp_ms <-
    Some
      (match t.max_timestamp_ms with
      | Some m -> max m timestamp_ms
      | None -> timestamp_ms)

let evict_retained t =
  match (t.max_timestamp_ms, t.retention_ms) with
  | Some max_timestamp_ms, Some retention_ms ->
      let cutoff = Saturating.sub max_timestamp_ms retention_ms in
      (* What a kept block reads is kept as long as it is: evicted sooner, a
         later recompute of the block would find less than it had. *)
      let competitor_cutoff = Saturating.sub cutoff adoption_window_ms in
      let context_cutoff = Saturating.sub cutoff context_reach_ms in
      t.contexts <-
        By_sequence.filter
          (fun _ entry -> entry.timestamp_ms >= context_cutoff)
          t.contexts;
      let expired, kept =
        Ids.partition
          (fun _ state -> state.produced.produced_at_ms < cutoff)
          t.produced_blocks
      in
      t.produced_blocks <- kept;
      Ids.iter
        (fun identity state ->
          t.competitors <- Ids.add identity state.produced t.competitors)
        expired;
      t.competitors <-
        Ids.filter
          (fun _ (produced : produced_block_observation) ->
            produced.produced_at_ms >= competitor_cutoff)
          t.competitors;
      t.status_keys <- Keys.filter (fun _ identity -> Ids.mem identity kept) t.status_keys
  | _ -> ()

(* An adoption of a block changes the rank of every block competing for its
   slot, kept or not: the kept ones are recomputed. *)
let mark_competitors_of_adopted t block_hash dirty =
  let adopted =
    List.map (fun (_, state) -> state.produced) (Ids.bindings t.produced_blocks)
    @ List.map snd (Ids.bindings t.competitors)
    |> List.filter (fun (produced : produced_block_observation) ->
           produced.block_hash = block_hash)
  in
  Ids.fold
    (fun identity state dirty ->
      if List.exists (fun produced -> same_slot_candidate produced state.produced) adopted
      then Id_set.add identity dirty
      else dirty)
    t.produced_blocks dirty

let mark_produced_blocks_affected_by_context t (context : context_entry) dirty =
  let from_ms = Saturating.sub context.timestamp_ms adoption_window_ms in
  let to_ms = Saturating.add context.timestamp_ms context_window_ms in
  Ids.fold
    (fun identity state dirty ->
      let at = state.produced.produced_at_ms in
      if at >= from_ms && at <= to_ms then Id_set.add identity dirty
      else
        let settle_height =
          Saturating.add state.produced.block_height progress_settle_blocks
        in
        let first_settlement_at_ms =
          match state.last_status with
          | Some { settlement_tip = Some tip; _ } -> Some tip.timestamp_ms
          | _ -> None
        in
        let improves_settlement_tip =
          match first_settlement_at_ms with
          | None -> true
          | Some timestamp_ms -> context.timestamp_ms < timestamp_ms
        in
        if
          context.timestamp_ms >= at
          && context.observation.height >= settle_height
          && ((not state.placement_finalized) || improves_settlement_tip)
        then Id_set.add identity dirty
        else dirty)
    t.produced_blocks dirty

let mark_related_produced_blocks t produced dirty =
  Ids.fold
    (fun identity state dirty ->
      if same_slot_candidate produced state.produced then Id_set.add identity dirty
      else dirty)
    t.produced_blocks dirty

let emit_dirty t dirty =
  let pending =
    Id_set.fold
      (fun identity pending ->
        match Ids.find_opt identity t.produced_blocks with
        | None -> pending
        | Some state ->
            let status = status t identity state.produced in
            let chain_placement = chain_placement t status in
            let status_changed = state.last_status <> Some status in
            let finalized_output_needed =
              chain_placement.placement_finalized && not state.finalized_output_emitted
            in
            if status_changed || finalized_output_needed then
              (identity, status, chain_placement, status_changed, finalized_output_needed)
              :: pending
            else pending)
      dirty []
    |> List.rev
  in
  List.concat_map
    (fun ( identity,
           (status : status),
           (chain_placement : chain_placement_result),
           status_changed,
           finalized_output_needed ) ->
      let placement_finalized = chain_placement.placement_finalized in
      let produced_at_ms = status.produced.produced_at_ms in
      let out =
        (if status_changed then [ status_output status ] else [])
        @
        if finalized_output_needed then
          [ finalized_output { identity; produced_at_ms; chain_placement } ]
        else []
      in
      (match Ids.find_opt identity t.produced_blocks with
      | None -> ()
      | Some state ->
          let state =
            {
              state with
              last_status = (if status_changed then Some status else state.last_status);
              placement_finalized = state.placement_finalized || placement_finalized;
              finalized_output_emitted =
                state.finalized_output_emitted || finalized_output_needed;
            }
          in
          t.produced_blocks <- Ids.add identity state t.produced_blocks);
      out)
    pending

(* Processes one record: what it derives, in order. *)
let process t (record : Record.t) =
  observe_timestamp t record.timestamp_ms;
  let dirty =
    match record.event with
    | Produced reported ->
        let produced = as_read reported record.timestamp_ms in
        let identity = Record.identity record in
        t.status_keys <- Keys.add (status_key identity) identity t.status_keys;
        t.produced_blocks <-
          Ids.add identity
            {
              produced;
              last_status = None;
              placement_finalized = false;
              finalized_output_emitted = false;
            }
            t.produced_blocks;
        mark_related_produced_blocks t produced Id_set.empty |> Id_set.add identity
    | Tip_adopted observation ->
        let entry =
          {
            sequence = t.next_sequence;
            sender = record.sender;
            run_id = record.run_id;
            timestamp_ms = record.timestamp_ms;
            observation;
          }
        in
        let dirty = mark_produced_blocks_affected_by_context t entry Id_set.empty in
        let dirty = mark_competitors_of_adopted t entry.observation.block_hash dirty in
        t.contexts <- By_sequence.add entry.sequence entry t.contexts;
        t.next_sequence <- t.next_sequence + 1;
        dirty
    | Other -> Id_set.empty
  in
  let out = emit_dirty t dirty in
  evict_retained t;
  out

(* The projection over [t], for a projector. *)
let projection t =
  Causet_projection.projection ?retention_ms:(retention_ms t) ~name (process t)
