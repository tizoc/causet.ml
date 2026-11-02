(* Generated logs of a chain and its nodes' reports: blocks every 5 seconds,
   sometimes two competing for a slot, at once, seconds apart or about the
   adoption window apart, sometimes one built on a stale tip; production
   reports timed at their production, as a producer sends them, in half the
   logs, and in the others some timed after it (seconds to minutes) or
   before it (within the clock skew tolerance or past it), which no producer
   sends, for the projection's determinism whatever it is fed; nodes with
   skewed clocks adopting them, some about five minutes late, around the
   adoption window's end, and restarting under new runs; idle stretches, and
   jumps of the newest time to the last competing pair's retention edge;
   stalls after which a block's production report arrives past its
   retention, settled only by reports kept longer; and reports that arrive
   late, some minutes late, a few past the retention. The log is in arrival
   order.

   Checked on every log:
   1. restart equivalence: restarted at the generated points, warmed up in
      log order, the projection outputs what the uninterrupted pass did;
   2. after every record, each retained block's last status is the status
      recomputed from every record so far;
   3. the contract: at most one finalized placement per block, and one for
      every block the chain progressed past while it was retained, or
      arrived settled by a report still kept.

   "No slot has two blocks finalized canonical" is not part of it: a
   competitor whose production report arrives late can outrank a block
   already finalized ([test_findings.ml]).

   Every draw is sequenced with [let]: OCaml leaves the order in which a
   function's arguments or a tuple's parts are evaluated unspecified, and
   the draws must come in the Rust port's order. *)

open Causet_test
open Chain_placement
open Record
open Classify
module P = Projection
module Projector = Causet_projection.Projector

let start = 1_000_000_000
let slot_ms = 5_000
let minute = 60_000

type step =
  | Slots  (** One to three slots, a block in each. *)
  | Compete
      (** Two blocks competing for one slot, produced at once, seconds apart, or about the
          adoption window apart. *)
  | Stale  (** A block built on a stale tip. *)
  | Idle  (** No block for a while. *)
  | Edge  (** The newest time jumps to the last competing blocks' retention edge. *)
  | Restart  (** A node restarts, under a new run. *)
  | Stalled
      (** A block whose production report is held back; two slots settle it; the chain
          stalls past its retention, but not past what the reports that settled it are
          kept for; then the report arrives. *)

let steps =
  [
    (Slots, 6); (Compete, 2); (Stale, 1); (Idle, 1); (Edge, 1); (Restart, 1); (Stalled, 1);
  ]

type node = {
  name : string;
  mutable run : int;
  skew : int;  (** Its clock's offset from true time. *)
  mutable next_event : int;
}

type block = { hash : string; height : int; slot : int }

(* Two blocks that competed for a slot. *)
type competition = {
  at : int;
      (** When the first was produced, by true time, and how long after it the second was
          ([gap]). *)
  gap : int;
  older_ms : int;
      (** Their production times as read; the older's as claimed instead, half the time,
          for the edges it would have read as given. *)
  newer_ms : int;
  early_ms : int option;
      (** When another node adopted the first before its production, if one did. *)
  parent : block;  (** The tip they were built on. *)
  newer : block;  (** The newer block, and when its production report arrives. *)
  newer_arrival : int;
}

(* A report in flight: when it arrives, then its generation order. *)
type arrival = { mutable arrival : int; order : int; record : Record.t }

(* The chain, its nodes, and the reports in flight to the log. *)
type chain = {
  world : Rng.t;  (** The world's own stream: timings, delays, who adopts. *)
  nodes : node array;
  mutable now : int;
  mutable slot : int;
  mutable tip : block;
  mutable behind : block;  (** The tip's parent, for a block built on a stale tip. *)
  arrivals : arrival Dynarray.t;
  mutable last_competition : competition option;
      (** The last two blocks competing for a slot. *)
  lags : bool;
      (** Whether production reports are timed after their production: by up to 3 s, one
          in ten minutes later. A producer times its report at the production; an eighth
          of the logs. *)
  lead_limit : int;
      (** How far before the production it claims a production report can be timed: within
          the tolerance in an eighth of the logs, past it in a quarter. *)
  mutable restart_before : event_identity list;
      (** Records to restart just before, where an edge is about to be read. *)
  mutable edge_recomputes : (event_identity * string) list;
      (** Each Edge step's recompute report, and the newer block, which it must reach;
          newest first. *)
}

let new_chain rng =
  let world = Rng.split rng in
  let count = Rng.between world 3 4 in
  let nodes =
    Array.init count (fun n ->
        let skew = Rng.between world 0 2_000 - 1_000 in
        { name = Printf.sprintf "node-%d" n; run = 0; skew; next_event = 0 })
  in
  let lags, lead_limit =
    match Rng.below world 8 with
    | 0 | 1 | 2 | 3 -> (false, 0)
    | 4 -> (true, 0)
    | 5 -> (false, clock_skew_tolerance_ms)
    | _ -> (false, 4 * clock_skew_tolerance_ms)
  in
  let genesis = { hash = "genesis"; height = 0; slot = 0 } in
  {
    world;
    nodes;
    now = start;
    slot = 0;
    tip = genesis;
    behind = genesis;
    arrivals = Dynarray.create ();
    last_competition = None;
    lags;
    lead_limit;
    restart_before = [];
    edge_recomputes = [];
  }

let adopted block parent =
  Tip_adopted
    {
      block_hash = block.hash;
      parent_hash = Some parent.hash;
      height = block.height;
      global_slot = block.slot;
    }

(* A node's report of [event] at true time [at], arriving at [arrival]. *)
let report_arriving chain node at arrival event =
  let node = chain.nodes.(node) in
  node.next_event <- node.next_event + 1;
  let record : Record.t =
    {
      sender = node.name;
      run_id = Printf.sprintf "run-%d" node.run;
      event_id = node.next_event;
      timestamp_ms = at + node.skew;
      event;
    }
  in
  let order = Dynarray.length chain.arrivals in
  Dynarray.add_last chain.arrivals { arrival; order; record };
  Record.identity record

(* A node's report of [event], [after] ms from now by true time, sent to the
   log with a delay. *)
let report chain node after event =
  let w = chain.world in
  let delay =
    if Rng.chance w 1 30 then (15 * minute) + Rng.below w (15 * minute)
    else if Rng.chance w 1 10 then minute + Rng.below w (9 * minute)
    else Rng.below w 5_000
  in
  let at = chain.now + after in
  ignore (report_arriving chain node at (at + delay) event)

(* [producer] produces a block on [parent] in the current slot, [after] ms
   from now, and adopts it itself. Its report is timed at the production, or
   after it in a log with lags; in a log with leads, one report in three is
   timed up to the lead limit before it, and with [past], one in two past the
   tolerance. Returns the block, and its production time as the projection
   reads it and as claimed. *)
let produce chain producer parent after past =
  let block =
    {
      hash = Printf.sprintf "h%ds%dn%d" (parent.height + 1) chain.slot producer;
      height = parent.height + 1;
      slot = chain.slot;
    }
  in
  let produced =
    {
      produced_at_ms = chain.now + after + chain.nodes.(producer).skew;
      block_hash = block.hash;
      parent_hash = Some parent.hash;
      block_height = block.height;
      global_slot = block.slot;
    }
  in
  let tolerance = clock_skew_tolerance_ms in
  let w = chain.world in
  let lead =
    if chain.lags then
      if Rng.chance w 1 10 then -(minute + Rng.below w (4 * minute))
      else -Rng.below w 3_000
    else if past && chain.lead_limit > tolerance && Rng.chance w 1 2 then
      tolerance + 1 + Rng.below w (chain.lead_limit - tolerance)
    else if chain.lead_limit > 0 && Rng.chance w 1 3 then
      Rng.below w (chain.lead_limit + 1)
    else 0
  in
  let claimed = produced.produced_at_ms in
  let read = min claimed (claimed - lead + tolerance) in
  report chain producer (after - lead) (Produced produced);
  report chain producer (after + 10) (adopted block parent);
  (block, read, claimed)

(* Every node but the producers adopts one of [blocks] after a short while;
   one in ten misses it, and one in twenty, coming back online, adopts it
   about five minutes later, within seconds of the adoption window's end,
   where competing blocks' windows differ. Each block comes with how long
   after now it was produced. *)
let adopt chain blocks producers =
  let w = chain.world in
  for node = 0 to Array.length chain.nodes - 1 do
    if List.mem node producers || Rng.chance w 1 10 then ()
    else
      let block, parent, produced = blocks.(Rng.below w (Array.length blocks)) in
      let after =
        if Rng.chance w 1 20 then adoption_window_ms - 3_000 + Rng.below w 6_000
        else 100 + Rng.below w 3_000
      in
      report chain node (produced + after) (adopted block parent)
  done

let next_slot chain =
  chain.now <- chain.now + slot_ms;
  chain.slot <- chain.slot + 1

let producer chain rng = Rng.below rng (Array.length chain.nodes)

(* How much longer than blocks the projection keeps reports: as far as a kept
   block reads. *)
let reports_reach_ms =
  if context_window_ms > adoption_window_ms + clock_skew_tolerance_ms then
    context_window_ms
  else adoption_window_ms + clock_skew_tolerance_ms

(* Moves the newest event time to an edge of the last competition:
   [competitor], where the older stops being the newer's competitor. *)
let edge chain rng competitor =
  match chain.last_competition with
  | None -> ()
  | Some c ->
      let node = producer chain rng in
      let edge =
        if not competitor then
          (* The cutoff at most at the newer's production as read: it is
             still kept (with clock skews the range overshoots it by up to
             about 3.5 s, and without this bound 65 of 1,093 recomputes
             reached no block kept). *)
          let drawn =
            c.at + context_retention_ms - 1_500 + Rng.below rng (c.gap + 3_000)
          in
          min drawn (c.newer_ms + context_retention_ms - chain.nodes.(node).skew)
        else
          (* The newest record's own time, so the cutoff lands there: past
             the early adoption's reach, when there was one. *)
          let highest = min c.newer_ms (c.older_ms + adoption_window_ms) in
          let lowest =
            match c.early_ms with
            | None -> highest - 2_000
            | Some early -> min (early + adoption_window_ms + 1) highest
          in
          let cutoff = lowest + Rng.below rng (highest - lowest + 1) in
          cutoff + context_retention_ms - chain.nodes.(node).skew
      in
      (* Not before the newer block's production has arrived, nor when a
         report arriving first, timed later (a lagging one), has already
         taken the cutoff past it: the recompute could not reach it. *)
      let kept_until = c.newer_ms + context_retention_ms in
      let overtaken =
        Dynarray.exists
          (fun a -> a.arrival <= edge + 1_000 && a.record.timestamp_ms > kept_until)
          chain.arrivals
      in
      if edge <= chain.now || edge <= c.newer_arrival || overtaken then ()
      else (
        chain.now <- edge;
        (* Arriving now, before the recompute below: the cutoff has moved
           when it is read. *)
        let tip = chain.tip and behind = chain.behind in
        let now = chain.now in
        ignore (report_arriving chain node now now (adopted tip behind));
        let late = producer chain rng in
        let grandparent = { c.parent with hash = "earlier" } in
        (* Timed, by its sender's clock, a second before the newer block's
           production as read: within the windows it reads, so it is
           recomputed. *)
        let timed = c.newer_ms - 1_000 in
        let arrival = chain.now + 1_000 in
        let at = timed - chain.nodes.(late).skew in
        let recompute =
          report_arriving chain late at arrival (adopted c.parent grandparent)
        in
        (* What comes next arrives after it, even a report timed before its
           production. *)
        chain.now <- arrival + chain.lead_limit;
        chain.edge_recomputes <- (recompute, c.newer.hash) :: chain.edge_recomputes;
        if Rng.chance rng 1 2 then
          chain.restart_before <- recompute :: chain.restart_before)

(* A block in the current slot on the tip, adopted, and the new tip. *)
let extend chain rng =
  let producer = producer chain rng in
  let parent = chain.tip in
  let block, _, _ = produce chain producer parent 0 false in
  adopt chain [| (block, parent, 0) |] [ producer ];
  chain.behind <- parent;
  chain.tip <- block

let step chain rng = function
  | Slots ->
      for _ = 1 to Rng.between rng 1 3 do
        next_slot chain;
        extend chain rng
      done
  | Compete ->
      next_slot chain;
      let n = Array.length chain.nodes in
      let first = producer chain rng in
      let second = (first + 1 + Rng.below rng (n - 1)) mod n in
      let parent = chain.tip in
      let w = chain.world in
      (* Same-slot blocks compete up to the adoption window apart: its edge
         is drawn too, a few seconds either side, or reported up to a second
         inside it, where the newer is still kept when the older is barely a
         competitor. *)
      let gap, inside =
        match Rng.below w 8 with
        | 0 | 1 | 2 -> (0, None)
        | 3 | 4 -> (Rng.below w 30_000, None)
        | 5 -> (adoption_window_ms - 5_000 + Rng.below w 10_000, None)
        | _ -> (0, Some (Rng.below w 1_000))
      in
      let aimed = Option.is_some inside in
      let a, older_ms, claimed = produce chain first parent 0 aimed in
      (* Inside the window by the times as read, or, half the time, as
         claimed: the edges of a projection reading a report past the
         tolerance as given. *)
      let edge_ms = if Rng.chance rng 1 2 then claimed else older_ms in
      let skew_b = chain.nodes.(second).skew in
      let gap =
        match inside with
        | None -> gap
        | Some inside -> edge_ms + adoption_window_ms - inside - chain.now - skew_b
      in
      let b, newer_ms, _ = produce chain second parent gap false in
      (* Its production report, pushed before its own adoption. *)
      let newer_arrival =
        (Dynarray.get chain.arrivals (Dynarray.length chain.arrivals - 2)).arrival
      in
      (* Half the time, and always just inside the window, another node
         adopts the first up to the tolerance before its production as read,
         as its clock reads. *)
      let others =
        List.filter (fun node -> node <> first && node <> second) (List.init n Fun.id)
      in
      let early_ms =
        if others <> [] && (aimed || Rng.chance w 1 2) then (
          let node = List.nth others (Rng.below w (List.length others)) in
          let timed = older_ms - 1 - Rng.below w 1_900 in
          let at = timed - chain.nodes.(node).skew in
          let arrival = at + Rng.below w 5_000 in
          ignore (report_arriving chain node at arrival (adopted a parent));
          Some timed)
        else None
      in
      adopt chain [| (a, parent, 0); (b, parent, gap) |] [ first; second ];
      chain.last_competition <-
        Some
          {
            at = chain.now;
            gap;
            older_ms = edge_ms;
            newer_ms;
            early_ms;
            parent;
            newer = b;
            newer_arrival;
          };
      (* The next block decides; the chain goes on from the winner. *)
      chain.behind <- parent;
      chain.tip <- (if Rng.chance rng 1 2 then b else a);
      (* Half the ones just inside the window go straight to their
         competitor edge, before later steps take the newest time past it. *)
      if aimed && Rng.chance rng 1 2 then edge chain rng true
  | Stale ->
      next_slot chain;
      let producer = producer chain rng in
      ignore (produce chain producer chain.behind 0 false)
  | Idle -> chain.now <- chain.now + (Rng.between rng 1 30 * minute)
  (* The newest event time moves to an edge of the last competing blocks'
     retention: between their two production times (give or take a second or
     two), where one is no longer kept and the other still is; or up to four
     seconds inside where the older stops being the newer's competitor, the
     newer still kept. Then a report from their slot arrives, which
     recomputes the one kept, and half the time the projection restarts just
     before it. A random idle stretch would rarely land there. *)
  | Edge ->
      let competitor = Rng.chance rng 1 2 in
      edge chain rng competitor
  | Restart ->
      let node = chain.nodes.(producer chain rng) in
      node.run <- node.run + 1;
      node.next_event <- 0
  (* Only the reports before the block's own, past the blocks' retention but
     still kept, settle it (without this step, nothing generated needed the
     contract to look that far). *)
  | Stalled ->
      next_slot chain;
      let held = Dynarray.length chain.arrivals in
      extend chain rng;
      for _ = 1 to progress_settle_blocks do
        next_slot chain;
        extend chain rng
      done;
      chain.now <-
        chain.now + context_retention_ms + 1_000 + Rng.below rng (reports_reach_ms - 5_000);
      (* A report that settles nothing moves the newest time. *)
      let node = producer chain rng in
      let now = chain.now in
      ignore (report_arriving chain node now now Other);
      (Dynarray.get chain.arrivals held).arrival <- now + 1_000;
      chain.now <- chain.now + 1_000 + chain.lead_limit

(* The log: every report, in arrival order. *)
let log chain =
  let arrivals = Dynarray.to_list chain.arrivals in
  Dynarray.clear chain.arrivals;
  List.sort (fun a b -> compare (a.arrival, a.order) (b.arrival, b.order)) arrivals
  |> List.map (fun a -> a.record)

let fresh () =
  let projector = Projector.create ~time:Record.time_ms in
  Projector.add projector (P.projection (P.create ()));
  projector

module Ints = Set.Make (Int)

(* A generated log and its restart points. *)
let generate rng =
  let chain = new_chain rng in
  let swarm = Swarm.create rng steps in
  while Rng.more rng do
    let s = Swarm.pick swarm rng in
    step chain rng s
  done;
  let log = log chain in
  let restarts = ref Ints.empty in
  for _ = 1 to Rng.below rng 4 do
    restarts := Ints.add (Rng.below rng (List.length log + 1)) !restarts
  done;
  List.iteri
    (fun position record ->
      if List.mem (Record.identity record) chain.restart_before then
        restarts := Ints.add position !restarts)
    log;
  (log, Ints.elements !restarts)

(* Check 2: what the projection stores for each kept block is what every
   record so far gives: the same projection keeping everything, asked for
   the block's status from scratch. *)
let incremental_equals_scratch log =
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
      fail ~key:"incremental"
        (Format.asprintf "%a" (Causet_projection.pp_mismatch P.pp_output) m)

(* The finalized keys of the blocks the chain progressed past while they were
   retained: a report two heights above, timed at or after the block,
   processed while the block was still kept, or kept when the block arrived
   (reports are kept [reach], [reports_reach_ms], past blocks). *)
let settled_while_retained log reach =
  let log = Array.of_list log in
  let newest = Array.make (Array.length log) 0 in
  let max = ref min_int in
  Array.iteri
    (fun i (record : Record.t) ->
      max := Stdlib.max !max record.timestamp_ms;
      newest.(i) <- !max)
    log;
  let cutoff_before position =
    if position = 0 then min_int else newest.(position - 1) - context_retention_ms
  in
  let settled = ref [] in
  Array.iteri
    (fun b (record : Record.t) ->
      match record.event with
      | Produced produced ->
          (* As the projection reads it: no later than the tolerance after its
             record's own time. *)
          let produced_at =
            min produced.produced_at_ms (record.timestamp_ms + clock_skew_tolerance_ms)
          in
          let settles = ref false in
          Array.iteri
            (fun c (context : Record.t) ->
              match context.event with
              | Tip_adopted tip ->
                  if
                    tip.height >= produced.block_height + progress_settle_blocks
                    && context.timestamp_ms >= produced_at
                    &&
                    if c > b then produced_at >= cutoff_before c
                    else context.timestamp_ms >= cutoff_before b - reach
                  then settles := true
              | _ -> ())
            log;
          if !settles then
            let id = Record.identity record in
            settled := finalized_key id.sender id.run_id id.event_id :: !settled
      | _ -> ())
    log;
  List.rev !settled

module Keys = Map.Make (String)

(* Check 3: the output's contract, on the uninterrupted pass. *)
let contract log =
  let projector = fresh () in
  let finalized = ref Keys.empty in
  List.iter
    (fun record ->
      List.iter
        (function
          | P.Finalized { key; finalized = placement } ->
              if Keys.mem key !finalized then
                fail ~key:"finalized twice" (key ^ " finalized twice");
              finalized := Keys.add key placement.chain_placement !finalized
          | P.Status _ -> ())
        (Projector.process projector record))
    log;
  let settled = settled_while_retained log reports_reach_ms in
  List.iter
    (fun key ->
      if not (Keys.mem key !finalized) then
        fail ~key:"settled, never finalized"
          (key ^ " was settled while retained, and never finalized"))
    settled;
  (* And only those (without this, a projection finalizing every block
     passed). *)
  Keys.iter
    (fun key (placement : chain_placement_result) ->
      if not (placement.placement_finalized && List.mem key settled) then
        fail ~key:"finalized, never settled"
          (key ^ " was finalized, and never settled while retained"))
    !finalized

let generated rng =
  let log, restarts = generate rng in
  (match Causet_projection.check_restarts fresh log restarts with
  | Ok () -> ()
  | Error d ->
      fail ~key:"restarts"
        (Format.asprintf "%a" (Causet_projection.pp_difference P.pp_output) d));
  incremental_equals_scratch log;
  contract log

(* The Rust port runs 2,000. *)
let runs = 2_000

let generated_logs_keep_restart_equivalence_and_the_contract () =
  check ~config:{ default with runs } generated

(* The generator itself: every Edge step's recompute reaches the newer block,
   read from the log at the recompute: arrived, still kept, and within the
   windows the recompute marks (timed from the older block, one in eleven was
   not within them; and a check that only restated the timing let 151 of
   1,431 reach a block evicted or not yet arrived). The windows hold by how
   the recompute is timed; arrived and kept are what this tests. *)
let every_edge_recompute_reaches_the_kept_block () =
  let edges = ref 0 and missed = ref 0 in
  for seed = 0 to 1_999 do
    let rng = Rng.create seed in
    let chain = new_chain rng in
    let swarm = Swarm.create rng steps in
    while Rng.more rng do
      let s = Swarm.pick swarm rng in
      step chain rng s
    done;
    let recomputes = List.rev chain.edge_recomputes in
    chain.edge_recomputes <- [];
    let log = Array.of_list (log chain) in
    List.iter
      (fun (identity, newer) ->
        incr edges;
        let r =
          match
            Array.find_index (fun record -> Record.identity record = identity) log
          with
          | Some r -> r
          | None -> Alcotest.fail "the recompute is in the log"
        in
        let newest = ref min_int in
        for i = 0 to r do
          newest := max !newest log.(i).timestamp_ms
        done;
        let timed = log.(r).timestamp_ms in
        let reached = ref false in
        for i = 0 to r - 1 do
          let record = log.(i) in
          match record.event with
          | Produced produced ->
              let read =
                min produced.produced_at_ms (record.timestamp_ms + clock_skew_tolerance_ms)
              in
              if
                produced.block_hash = newer
                && read >= !newest - context_retention_ms
                && read >= timed - adoption_window_ms
                && read <= timed + context_window_ms
              then reached := true
          | _ -> ()
        done;
        if not !reached then incr missed)
      recomputes
  done;
  if !edges <= 1_000 then Alcotest.failf "only %d Edge steps" !edges;
  Alcotest.(check int)
    (Printf.sprintf "%d of %d Edge recomputes miss the newer block" !missed !edges)
    0 !missed

(* The generator itself: an Edge whose newer block a report arriving first,
   timed later, has already evicted is skipped (that report comes about once
   in 300,000 logs, so no run of seeds would see the skip removed). *)
let an_edge_past_an_overtaking_report_is_skipped () =
  List.iter
    (fun overtaking ->
      let rng = Rng.create 0 in
      let chain = new_chain rng in
      let parent = chain.tip in
      let newer = { hash = "newer"; height = 1; slot = 1 } in
      chain.last_competition <-
        Some
          {
            at = start;
            gap = 1_000;
            older_ms = start;
            newer_ms = start + 1_000;
            early_ms = None;
            parent;
            newer;
            newer_arrival = start + 1_000;
          };
      (if overtaking then
         let past = start + 1_000 + context_retention_ms + 1 - chain.nodes.(0).skew in
         ignore (report_arriving chain 0 past (start + 2_000) (adopted parent parent)));
      edge chain rng false;
      Alcotest.(check int)
        (Printf.sprintf "overtaking %b" overtaking)
        (if overtaking then 0 else 1)
        (List.length chain.edge_recomputes))
    [ false; true ]

(* The generator itself: Stalled steps make blocks that only reports kept past
   the blocks' retention settle, which the contract must then require
   (without them no generated log had one, and only the hand-written test
   below tested the contract's reach). *)
let stalled_blocks_are_settled_only_by_reports_kept_longer () =
  let reached = ref 0 in
  for seed = 0 to 999 do
    let rng = Rng.create seed in
    let log, _ = generate rng in
    let within = List.length (settled_while_retained log 0) in
    let kept = List.length (settled_while_retained log reports_reach_ms) in
    if kept > within then incr reached
  done;
  (* Stalled is on in half the swarms, and then about one step in five: some
     1,600 Stalled steps in 1,000 logs. *)
  if !reached < 200 then Alcotest.failf "only %d of 1,000 logs need the reach" !reached

(* The contract model: a block arriving past retention, settled by a report
   still kept, is finalized (a model that asked it only of reports within the
   blocks' retention missed it). *)
let the_contract_requires_a_block_settled_by_a_report_still_kept () =
  let record sender timestamp_ms event : Record.t =
    { sender; run_id = "run"; event_id = 1; timestamp_ms; event }
  in
  let tip : chain_block_context_observation =
    { block_hash = "h3"; parent_hash = Some "h2"; height = 3; global_slot = 3 }
  in
  let block : produced_block_observation =
    {
      produced_at_ms = 0;
      block_hash = "h1";
      parent_hash = Some "h0";
      block_height = 1;
      global_slot = 1;
    }
  in
  let log =
    [
      record "observer" 50_000 (Tip_adopted tip);
      record "clock" (context_retention_ms + 100_000) Other;
      record "producer" 0 (Produced block);
    ]
  in
  let projector = fresh () in
  let finalized =
    List.concat_map (Projector.process projector) log
    |> List.filter_map (function
         | P.Finalized { key; _ } -> Some key
         | P.Status _ -> None)
  in
  let key = finalized_key "producer" "run" 1 in
  Alcotest.(check (list string)) "the projection finalizes it" [ key ] finalized;
  Alcotest.(check bool)
    "the model requires it" true
    (List.mem key (settled_while_retained log reports_reach_ms))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "chain_placement generated"
    [
      ( "generated",
        [
          Alcotest.test_case "generated logs keep restart equivalence and the contract"
            `Slow generated_logs_keep_restart_equivalence_and_the_contract;
        ] );
      ( "generator",
        [
          t "every edge recompute reaches the kept block"
            every_edge_recompute_reaches_the_kept_block;
          t "an edge past an overtaking report is skipped"
            an_edge_past_an_overtaking_report_is_skipped;
          t "stalled blocks are settled only by reports kept longer"
            stalled_blocks_are_settled_only_by_reports_kept_longer;
          t "the contract requires a block settled by a report still kept"
            the_contract_requires_a_block_settled_by_a_report_still_kept;
        ] );
    ]
