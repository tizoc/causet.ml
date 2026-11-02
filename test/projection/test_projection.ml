(* The projection crate's own example: an id seen again within a minute of
   its last sighting, under restarts after every record and with a record
   arriving long after its time. *)

open Causet_projection

type seen = { at : int; id : int }

let window = 60_000

(* Says when an id is seen again within a minute of its last sighting. *)
let repeats () =
  (* The newest event time seen: records arrive late and out of order, so the
     window is measured from here, never from the record at hand. *)
  let watermark = ref 0 in
  let module M = Map.Make (Int) in
  let last = ref M.empty in
  projection ~retention_ms:window ~name:"repeats" (fun seen ->
      watermark := max !watermark seen.at;
      let horizon = max 0 (!watermark - window) in
      (* Older than the window: a warm-up would not see it either. *)
      if seen.at < horizon then []
      else
        let out =
          match M.find_opt seen.id !last with
          | Some before when abs (seen.at - before) <= window ->
              [ Printf.sprintf "%d@%d" seen.id seen.at ]
          | _ -> []
        in
        let newest =
          match M.find_opt seen.id !last with Some b -> max b seen.at | None -> seen.at
        in
        last := M.filter (fun _ at -> at >= horizon) (M.add seen.id newest !last);
        out)

let fresh () =
  let p = Projector.create ~time:(fun s -> s.at) in
  Projector.add p (repeats ());
  p

let seen log = List.map (fun (at, id) -> { at; id }) log
let every log = List.init (List.length log - 1) (fun i -> i + 1)
let ok = function Ok () -> true | Error _ -> false

let restarting_after_any_record_changes_nothing () =
  let log = seen [ (0, 1); (10_000, 2); (20_000, 1); (90_000, 1); (100_000, 2) ] in
  Alcotest.(check (list string)) "first" [] (Projector.process (fresh ()) (List.hd log));
  Alcotest.(check bool) "restarts" true (ok (check_restarts fresh log (every log)))

let a_late_record_is_ignored_live_as_a_warm_up_ignores_it () =
  let late = seen [ (100_000, 1); (0, 1); (100_001, 1) ] in
  Alcotest.(check bool) "restarts" true (ok (check_restarts fresh late (every late)))

let () =
  Alcotest.run "projection"
    [
      ( "projection",
        [
          Alcotest.test_case "a restart after any record changes nothing" `Quick
            restarting_after_any_record_changes_nothing;
          Alcotest.test_case "a late record is ignored live as a warm-up ignores it"
            `Quick a_late_record_is_ignored_live_as_a_warm_up_ignores_it;
        ] );
    ]
