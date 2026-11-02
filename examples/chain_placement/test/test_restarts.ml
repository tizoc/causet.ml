(* Port of causet-projection's tests/restarts.rs. Restart equivalence on
   generated logs: a block adoption projection, with records arriving late
   and out of order, keeps it; a retention shorter than its context, and a
   warm-up that re-sorts the log by event time while live processing keeps
   log order, break it, and the check finds and shrinks each. *)

open Causet_projection
open Causet_test

(* How long a produced block waits for its adoption, in event time. *)
let window = 50

type kind = Produced | Adopted
type rec_ = { time : int; kind : kind; block : int }

(* A produced block's fate. *)
type placement = { key : string; adopted : bool }

let pp_chain_placement_result f p =
  Format.fprintf f "Placement { key: %S, adopted: %b }" p.key p.adopted

let pp_rec f r =
  Format.fprintf f "Rec { time: %d, kind: %s, block: %d }" r.time
    (match r.kind with Produced -> "Produced" | Adopted -> "Adopted")
    r.block

module Ints = Map.Make (Int)

(* Places each produced block: adopted if its adoption arrives while it is
   pending, orphaned once the newest event time seen is a window past it. *)
let adoption retention =
  (* The newest event time seen. *)
  let now = ref 0 in
  let pending = ref Ints.empty in
  projection ~retention_ms:retention ~name:"adoption" (fun record ->
      let place block adopted = { key = Printf.sprintf "block %d" block; adopted } in
      now := max !now record.time;
      let horizon = max 0 (!now - window) in
      let out =
        match record.kind with
        (* Too old to wait for: never pending. *)
        | Produced when record.time < horizon -> []
        | Produced ->
            pending := Ints.add record.block record.time !pending;
            []
        | Adopted ->
            if Ints.mem record.block !pending then (
              pending := Ints.remove record.block !pending;
              [ place record.block true ])
            else []
      in
      let expired, kept = Ints.partition (fun _ t -> t < horizon) !pending in
      pending := kept;
      out @ List.map (fun (b, _) -> place b false) (Ints.bindings expired))

let projector retention =
  let p = Projector.create ~time:(fun r -> r.time) in
  Projector.add p (adoption retention);
  p

(* A log: blocks produced as time goes on, some adopted a little later, each
   record arriving after a delay, so the log is in arrival order, not event
   order; and restarts, each at some step's arrival. A step's choices are
   drawn together, so shrinking that deletes them deletes the step. *)
let generate rng =
  let time = ref 0
  and next = ref 0
  and produced =
    ref []
    (* newest first *)
  in
  let arrivals = ref [] and restart_times = ref [] in
  let count = ref 0 in
  while Rng.more rng do
    time := !time + Rng.below rng 30;
    let record =
      if !produced = [] || not (Rng.chance rng 1 2) then (
        produced := !next :: !produced;
        incr next;
        { time = !time; kind = Produced; block = !next - 1 })
      else
        (* Counted back from the newest, so deleting an older step while
           shrinking does not change which block this adopts. *)
        let block = List.nth !produced (Rng.below rng (List.length !produced)) in
        { time = !time; kind = Adopted; block }
    in
    let arrival = !time + Rng.below rng 40 in
    if Rng.chance rng 1 4 then restart_times := arrival :: !restart_times;
    arrivals := (arrival, !count, record) :: !arrivals;
    incr count
  done;
  let arrivals = List.sort (fun (a, s, _) (b, t, _) -> compare (a, s) (b, t)) !arrivals in
  (* A restart at time t: before the first record arriving at t or later. *)
  let restarts =
    List.rev_map
      (fun t -> List.length (List.filter (fun (a, _, _) -> a < t) arrivals))
      !restart_times
  in
  (List.map (fun (_, _, r) -> r) arrivals, restarts)

(* The log a list of choices makes. *)
let log_of choices =
  let log = ref [] in
  replay choices (fun rng -> log := fst (generate rng));
  !log

let quick = { default with runs = 300 }

let show_found (found : found) =
  Format.asprintf "seed %d, key %s, choices [%s]: [%a]" found.seed found.key
    (String.concat "; " (List.map string_of_int found.choices))
    (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp_rec)
    (log_of found.choices)

let fail_difference d =
  fail ~key:"difference"
    (Format.asprintf "%a" (pp_difference pp_chain_placement_result) d)

let a_processor_whose_retention_covers_its_context_keeps_restart_equivalence () =
  check ~config:quick (fun rng ->
      let log, restarts = generate rng in
      match check_restarts (fun () -> projector window) log restarts with
      | Ok () -> ()
      | Error d -> fail_difference d)

let a_retention_shorter_than_the_context_is_found () =
  match
    search quick (fun rng ->
        let log, restarts = generate rng in
        match check_restarts (fun () -> projector (window / 2)) log restarts with
        | Ok () -> ()
        | Error d -> fail_difference d)
  with
  | None -> Alcotest.fail "a short retention breaks a restart"
  | Some found ->
      (* Shrunk to a few records and a restart. *)
      if List.length (log_of found.choices) > 4 then Alcotest.fail (show_found found)

let a_warm_up_sorted_by_event_time_is_found () =
  (* The warm-up reads the stored log in (timestamp, row) order while live
     processing reads it in row order: a different order. *)
  let sorted_warm_up prefix =
    let by_time = List.stable_sort (fun a b -> compare a.time b.time) prefix in
    let p = projector window in
    Projector.warm_up p by_time;
    p
  in
  match
    search quick (fun rng ->
        let log, restarts = generate rng in
        match
          check_restarts_with (fun () -> projector window) sorted_warm_up log restarts
        with
        | Ok () -> ()
        | Error d -> fail_difference d)
  with
  | None -> Alcotest.fail "re-sorting the warm-up breaks a restart"
  | Some found ->
      if List.length (log_of found.choices) > 4 then Alcotest.fail (show_found found)

(* ------------------------------------------------------------------------ *)
(* The projector's own contracts *)

(* Outputs one placement per record, keyed by the projection and the block. *)
let echo name retention_ms =
  projection ?retention_ms ~name (fun record ->
      [ { key = Printf.sprintf "%s %d" name record.block; adopted = true } ])

let rec_ time block = { time; kind = Produced; block }

let processors_run_in_the_order_added () =
  let p = Projector.create ~time:(fun r -> r.time) in
  Projector.add p (echo "first" None);
  Projector.add p (echo "second" None);
  let keys = List.map (fun p -> p.key) (Projector.process p (rec_ 0 7)) in
  Alcotest.(check (list string)) "keys" [ "first 7"; "second 7" ] keys;
  Alcotest.(check (list string)) "names" [ "first"; "second" ] (Projector.names p)

let each_processor_warms_up_on_its_own_window_and_outputs_nothing () =
  let fed = ref [] in
  (* Records the blocks it is fed. *)
  let fed_projection name retention =
    projection ~retention_ms:retention ~name (fun record ->
        fed := (name, record.block) :: !fed;
        [ { key = ""; adopted = true } ])
  in
  let p = Projector.create ~time:(fun r -> r.time) in
  Projector.add p (fed_projection "short" 10);
  Projector.add p (fed_projection "long" 100);
  Projector.add p (echo "none" None);
  (* Log order, not time order: 95 arrived before 80. *)
  let log = [ rec_ 0 0; rec_ 95 1; rec_ 80 2; rec_ 100 3 ] in
  Alcotest.(check (option int))
    "the longest window starts at 0" (Some 0)
    (Projector.warm_up_from p 100);
  List.iter (Projector.warm p) log;
  Alcotest.(check (list (pair string int)))
    "short sees 90..; long sees all; none, without context, nothing"
    [ ("long", 0); ("short", 1); ("long", 1); ("long", 2); ("short", 3); ("long", 3) ]
    (List.rev !fed)

let a_difference_without_a_restart_means_the_processor_is_not_deterministic () =
  (* Outputs a number that differs between instances. *)
  let instances = ref 0 in
  let fresh () =
    let instance = !instances in
    incr instances;
    let p = Projector.create ~time:(fun r -> r.time) in
    Projector.add p
      (projection ~name:"counter" (fun _ ->
           [ { key = string_of_int instance; adopted = true } ]));
    p
  in
  match check_restarts fresh [ rec_ 0 0 ] [] with
  | Ok () -> Alcotest.fail "two instances differ"
  | Error { restart; record; _ } ->
      Alcotest.(check (pair (option int) int))
        "restart, record" (None, 0) (restart, record)

(* ------------------------------------------------------------------------ *)
(* The crate's documented example, under generated logs: its hand-picked
   logs missed a late record that broke it. *)

let minute = 60_000

type seen = { at : int; id : int }

(* As in the crate's documentation. *)
let repeats () =
  let watermark = ref 0 in
  let last = ref Ints.empty in
  projection ~retention_ms:minute ~name:"repeats" (fun seen ->
      watermark := max !watermark seen.at;
      let horizon = max 0 (!watermark - minute) in
      if seen.at < horizon then []
      else
        let out =
          match Ints.find_opt seen.id !last with
          | Some before when abs (seen.at - before) <= minute ->
              [ Printf.sprintf "%d@%d" seen.id seen.at ]
          | _ -> []
        in
        let newest =
          match Ints.find_opt seen.id !last with
          | Some b -> max b seen.at
          | None -> seen.at
        in
        last := Ints.filter (fun _ at -> at >= horizon) (Ints.add seen.id newest !last);
        out)

let the_documented_example_keeps_restart_equivalence_with_late_records () =
  check ~config:quick (fun rng ->
      let time = ref 0 in
      let log = ref [] and restarts = ref [] and length = ref 0 in
      while Rng.more rng do
        time := !time + Rng.below rng 40_000;
        (* Some records arrive long after their time. *)
        let at =
          if Rng.chance rng 1 4 then max 0 (!time - Rng.below rng (3 * minute)) else !time
        in
        let id = Rng.below rng 3 in
        log := { at; id } :: !log;
        incr length;
        if Rng.chance rng 1 3 then restarts := !length :: !restarts
      done;
      let fresh () =
        let p = Projector.create ~time:(fun s -> s.at) in
        Projector.add p (repeats ());
        p
      in
      match check_restarts fresh (List.rev !log) (List.rev !restarts) with
      | Ok () -> ()
      | Error d ->
          fail ~key:"difference"
            (Format.asprintf "%a" (pp_difference Format.pp_print_string) d))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "projection restarts"
    [
      ( "restarts",
        [
          t "a processor whose retention covers its context keeps restart equivalence"
            a_processor_whose_retention_covers_its_context_keeps_restart_equivalence;
          t "a retention shorter than the context is found"
            a_retention_shorter_than_the_context_is_found;
          t "a warm-up sorted by event time is found"
            a_warm_up_sorted_by_event_time_is_found;
        ] );
      ( "projector",
        [
          t "processors run in the order added" processors_run_in_the_order_added;
          t "each processor warms up on its own window and outputs nothing"
            each_processor_warms_up_on_its_own_window_and_outputs_nothing;
          t "a difference without a restart means the processor is not deterministic"
            a_difference_without_a_restart_means_the_processor_is_not_deterministic;
        ] );
      ( "documented example",
        [
          t "the documented example keeps restart equivalence with late records"
            the_documented_example_keeps_restart_equivalence_with_late_records;
        ] );
    ]
