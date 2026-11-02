(* What generation found in chain placement, each as the shortest story that
   shows it. *)

open Chain_placement
open Record
module P = Projection

let record sender event_id timestamp_ms event : Record.t =
  { sender; run_id = "run"; event_id; timestamp_ms; event }

let adopted hash parent_hash height =
  Tip_adopted
    { block_hash = hash; parent_hash = Some parent_hash; height; global_slot = height }

let produced at hash parent_hash height =
  Produced
    {
      produced_at_ms = at;
      block_hash = hash;
      parent_hash = Some parent_hash;
      block_height = height;
      global_slot = height;
    }

let process_all projection log = List.concat_map (P.process projection) log

(* The canonical placements finalized, by block. *)
let finalized_canonical out =
  List.filter_map
    (function
      | P.Finalized { finalized; _ } when finalized.chain_placement.canonical ->
          Some finalized.chain_placement.block_hash
      | _ -> None)
    out

(* Finality comes from chain progress, but a slot's ranking can still change
   after it: a competitor whose production report arrives late, with a later
   external adoption, is finalized canonical beside the block already
   finalized canonical for the same slot. The original behaves so; this pins
   it. *)
let a_late_production_report_finalizes_a_second_canonical_block_for_its_slot () =
  let log =
    [
      record "a" 1 1_000 (adopted "parent" "root" 1);
      (* B is produced, adopted by an observer, and settled by progress. *)
      record "b" 1 5_000 (produced 5_000 "B" "parent" 2);
      record "observer" 1 5_500 (adopted "B" "parent" 2);
      (* A's adoption by another observer arrives, later than B's... *)
      record "watcher" 1 6_000 (adopted "A" "parent" 2);
      record "observer" 2 15_000 (adopted "next" "B" 4);
      (* ...and A's own production report only now. *)
      record "a" 2 4_900 (produced 4_900 "A" "parent" 2);
    ]
  in
  let out = process_all (P.create ()) log in
  Alcotest.(check (list string)) "canonical" [ "B"; "A" ] (finalized_canonical out)

(* Context is evicted 15 minutes behind the newest event time, but a block
   kept that long reads context up to 5 minutes before it. When the newest
   time jumps ahead, a kept block's parent reports are gone; a late, older
   parent report then recomputes it from less than it had, and its stored
   status loses the newer report: with the original's eviction, the last
   status names the report at 1,000. Section 25.7 keeps context as far back
   as a kept block reads. *)
let a_kept_block_is_not_recomputed_from_evicted_context () =
  let minute = 60_000 in
  let log =
    [
      record "a" 1 0 (adopted "parent" "root" 1);
      record "b" 1 4_000 (adopted "parent" "root" 1);
      record "a" 2 (3 * minute) (produced (3 * minute) "block" "parent" 2);
      (* The newest time jumps 12 minutes past the block. *)
      record "c" 1 ((15 * minute) + 30_000) (adopted "far" "elsewhere" 9);
      (* A late and older report of the parent. *)
      record "c" 2 1_000 (adopted "parent" "root" 1);
    ]
  in
  let parent_seen_at =
    process_all (P.create ()) log
    |> List.filter_map (function
         | P.Status { status; _ } when status.produced.block_hash = "block" ->
             Some status.parent_seen_at_ms
         | _ -> None)
  in
  let last = match List.rev parent_seen_at with x :: _ -> Some x | [] -> None in
  Alcotest.(check (option (option int)))
    "the newest parent report stays the block's" (Some (Some 4_000)) last

(* What the projection stores agrees with every record so far. *)
let agrees_with_every_record log =
  let kept = P.create () and everything = P.unbounded () in
  match
    Causet_projection.check_incremental ~key:P.key ~projection:(P.process kept)
      ~reference:(fun r -> ignore (P.process everything r))
      ~recompute:(fun key ->
        match P.status_now kept key with
        | None -> None
        | Some _ -> P.status_now everything key)
      log
  with
  | Ok () -> ()
  | Error m ->
      Alcotest.fail (Format.asprintf "%a" (Causet_projection.pp_mismatch P.pp_output) m)

let competing at hash =
  Produced
    {
      produced_at_ms = at;
      block_hash = hash;
      parent_hash = Some "P";
      block_height = 2;
      global_slot = 2;
    }

let retention = Classify.context_retention_ms

(* Regression: a kept block's competitors are part of what it reads. A,
   produced a second before B, leaves the kept blocks first; a later
   recompute of B must still count it. *)
let a_kept_block_keeps_the_competitor_its_rank_depends_on () =
  agrees_with_every_record
    [
      record "producer-a" 1 0 (competing 0 "A");
      record "producer-b" 1 1_000 (competing 1_000 "B");
      record "clock" 1 (retention + 500) Other;
      record "observer" 1 500 (adopted "P" "root" 1);
    ]

(* Regression: an adoption of A, inside A's adoption window but past B's
   marking window, changes B's rank: B is recomputed too. *)
let adopting_a_block_recomputes_the_other_blocks_of_its_slot () =
  agrees_with_every_record
    [
      record "producer-b" 1 0 (competing 0 "B");
      record "producer-a" 1 1_000 (competing 1_000 "A");
      record "observer" 1 300_500 (adopted "A" "P" 2);
    ]

(* Regression: competitors up to five minutes apart compete. A, five minutes
   older than B, leaves the kept blocks first; B, still kept, must keep
   counting it. (Generation made competitors at most a couple of seconds
   apart, so it passed with them kept 3 seconds.) *)
let a_competitor_five_minutes_older_stays_a_competitor () =
  agrees_with_every_record
    [
      record "producer-a" 1 0 (competing 0 "A");
      record "producer-b" 1 300_000 (competing 300_000 "B");
      record "clock" 1 (retention + 200_000) Other;
      record "observer" 1 299_000 (adopted "P" "root" 1);
    ]

(* Regression: a competitor five minutes older than a kept block, adopted a
   second before its own production, within the clock skew: that adoption is
   still read, so it is kept the skew longer than the window. *)
let a_competitor_s_early_adoption_stays_in_context () =
  agrees_with_every_record
    [
      record "producer-a" 1 10_000 (competing 10_000 "A");
      record "producer-b" 1 310_000 (competing 310_000 "B");
      record "observer" 1 9_000 (adopted "A" "P" 2);
      record "clock" 1 (retention + 310_000) Other;
      record "observer" 2 309_000 (adopted "P" "root" 1);
    ]

(* Regression: competitors ten seconds apart, and an adoption of A at the end
   of A's window, nine seconds past B's: B is recomputed. *)
let adopting_a_competitor_ten_seconds_newer_recomputes_the_older () =
  agrees_with_every_record
    [
      record "producer-b" 1 0 (competing 0 "B");
      record "producer-a" 1 10_000 (competing 10_000 "A");
      record "observer" 1 309_000 (adopted "A" "P" 2);
    ]

(* The status a block was last output with, over [log]. *)
let last_status log hash =
  process_all (P.create ()) log
  |> List.rev
  |> List.find_map (function
       | P.Status { status; _ } when status.produced.block_hash = hash -> Some status
       | _ -> None)
  |> function
  | Some s -> s
  | None -> Alcotest.fail "a status"

(* Regression: a production report timed three seconds before the production
   it claims, past the skew tolerance. Blocks were kept by production time
   and replayed by record time, so a warm-up missed A, and after a restart
   B's next status no longer counted it. A production is now read as no
   later than the tolerance after its report's own time: A as produced at
   9,000, 301 seconds before B, so it competes with B in neither pass: the
   story above no longer happens, which is the fix. *)
let a_production_claimed_after_its_report_competes_alike_after_a_restart () =
  let log =
    [
      record "producer-a" 1 7_000 (competing 10_000 "A");
      record "producer-b" 1 310_000 (competing 310_000 "B");
      record "clock" 1 (retention + 310_000) Other;
      record "observer" 1 309_000 (adopted "P" "root" 1);
    ]
  in
  let fresh () =
    let projector = Causet_projection.Projector.create ~time:Record.time_ms in
    Causet_projection.Projector.add projector (P.projection (P.create ()));
    projector
  in
  (match Causet_projection.check_restarts fresh log [ 3 ] with
  | Ok () -> ()
  | Error d ->
      Alcotest.fail (Format.asprintf "%a" (Causet_projection.pp_difference P.pp_output) d));
  let b = last_status log "B" in
  Alcotest.(check int) "A, read at 9,000, does not compete" 1 b.slot_block_count

(* Regression: no test or generated log had a production report timed after
   its production, so reading such a report differently passed every suite.
   (A producer times its report at the production; one timed later is no
   more than a projection must read alike.) Injected at 1,000, reported a
   minute later, adopted at 1,500: read at its production. *)
let a_production_reported_a_minute_late_is_read_at_its_production () =
  let log =
    [
      record "observer" 1 1_500 (adopted "A" "P" 2);
      record "producer" 1 61_000 (competing 1_000 "A");
    ]
  in
  let status = last_status log "A" in
  Alcotest.(check int) "produced at" 1_000 status.produced.produced_at_ms;
  Alcotest.(check int) "senders" 1 status.adoption_sender_count;
  Alcotest.(check (option int)) "first adopted" (Some 1_500) status.first_adopted_at_ms

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "chain_placement findings"
    [
      ( "findings",
        [
          t "a late production report finalizes a second canonical block for its slot"
            a_late_production_report_finalizes_a_second_canonical_block_for_its_slot;
          t "a kept block is not recomputed from evicted context"
            a_kept_block_is_not_recomputed_from_evicted_context;
          t "a kept block keeps the competitor its rank depends on"
            a_kept_block_keeps_the_competitor_its_rank_depends_on;
          t "adopting a block recomputes the other blocks of its slot"
            adopting_a_block_recomputes_the_other_blocks_of_its_slot;
          t "a competitor five minutes older stays a competitor"
            a_competitor_five_minutes_older_stays_a_competitor;
          t "a competitor's early adoption stays in context"
            a_competitor_s_early_adoption_stays_in_context;
          t "adopting a competitor ten seconds newer recomputes the older"
            adopting_a_competitor_ten_seconds_newer_recomputes_the_older;
          t "a production claimed after its report competes alike after a restart"
            a_production_claimed_after_its_report_competes_alike_after_a_restart;
          t "a production reported a minute late is read at its production"
            a_production_reported_a_minute_late_is_read_at_its_production;
        ] );
    ]
