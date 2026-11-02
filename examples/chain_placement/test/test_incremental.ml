(* Port of causet-projection's tests/incremental.rs. Incremental equals from
   scratch on generated logs: each id's newest value by event time, with
   records arriving late and out of order. Kept by event time, it passes;
   overwritten in arrival order, the check finds it and shrinks it to its
   story: two records of one id, the newer arriving first. *)

open Causet_projection
open Causet_test

type rec_ = { time : int; id : int; value : int }

(* An id's newest value, by event time. *)
type newest = { key : string; time : int; value : int }

let pp_newest f n =
  Format.fprintf f "Newest { key: %S, time: %d, value: %d }" n.key n.time n.value

let pp_rec f (r : rec_) =
  Format.fprintf f "Rec { time: %d, id: %d, value: %d }" r.time r.id r.value

let key id = Printf.sprintf "id %d" id

module Ints = Map.Make (Int)

(* Each id's newest record; [by_event_time] false overwrites with every
   record, in arrival order: the mistake to find. *)
let latest by_event_time =
  let newest = ref Ints.empty in
  fun (record : rec_) ->
    let older =
      match Ints.find_opt record.id !newest with
      | Some (time, _) -> time > record.time
      | None -> false
    in
    if by_event_time && older then []
    else (
      newest := Ints.add record.id (record.time, record.value) !newest;
      [ { key = key record.id; time = record.time; value = record.value } ])

(* Every record, kept: the definition, recomputed from scratch. *)
type history = { mutable records : rec_ list (* newest first *) }

let history () = { records = [] }
let keep h record = h.records <- record :: h.records

(* The newest record of the key's id; of two at one time, the later to
   arrive. *)
let newest h k =
  List.rev h.records
  |> List.mapi (fun arrived r -> (arrived, r))
  |> List.filter (fun (_, (r : rec_)) -> key r.id = k)
  |> List.fold_left
       (fun best (arrived, (r : rec_)) ->
         match best with
         | Some (a, (b : rec_)) when compare (r.time, arrived) (b.time, a) < 0 -> best
         | _ -> Some (arrived, r))
       None
  |> Option.map (fun (_, (r : rec_)) -> { key = k; time = r.time; value = r.value })

(* Records of a few ids as time goes on, each arriving after a delay: the log
   is in arrival order. *)
let generate rng =
  let time = ref 0 and arrivals = ref [] and count = ref 0 in
  while Rng.more rng do
    time := !time + Rng.below rng 10;
    let id = Rng.below rng 3 in
    let value = Rng.below rng 100 in
    let record = { time = !time; id; value } in
    let arrival = !time + Rng.below rng 30 in
    arrivals := (arrival, !count, record) :: !arrivals;
    incr count
  done;
  List.sort (fun (a, s, _) (b, t, _) -> compare (a, s) (b, t)) !arrivals
  |> List.map (fun (_, _, r) -> r)

let check_log ?(recompute = fun h k -> newest h k) by_event_time log =
  let h = history () in
  check_incremental
    ~key:(fun n -> n.key)
    ~projection:(latest by_event_time) ~reference:(keep h) ~recompute:(recompute h) log

let quick = { default with runs = 300 }

let a_projection_kept_by_event_time_equals_its_recomputation () =
  check ~config:quick (fun rng ->
      let log = generate rng in
      match check_log true log with
      | Ok () -> ()
      | Error m -> fail ~key:"mismatch" (Format.asprintf "%a" (pp_mismatch pp_newest) m))

let a_projection_overwritten_in_arrival_order_is_found_and_shrunk () =
  match
    search quick (fun rng ->
        let log = generate rng in
        match check_log false log with
        | Ok () -> ()
        | Error m -> fail ~key:"mismatch" (Format.asprintf "%a" (pp_mismatch pp_newest) m))
  with
  | None -> Alcotest.fail "arrival order loses the newest value"
  | Some found ->
      let log = ref [] in
      replay found.choices (fun rng -> log := generate rng);
      let shown =
        Format.asprintf "choices [%s]: [%a]"
          (String.concat "; " (List.map string_of_int found.choices))
          (Format.pp_print_list
             ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
             pp_rec)
          !log
      in
      Alcotest.(check int) shown 2 (List.length !log);
      let first = List.nth !log 0 and second = List.nth !log 1 in
      Alcotest.(check bool)
        ("the newer arrives first: " ^ shown)
        true (first.time > second.time)

let two_of_one_id = [ { time = 5; id = 1; value = 50 }; { time = 2; id = 1; value = 20 } ]

let a_mismatch_names_the_record_the_key_and_both_outputs () =
  match check_log false two_of_one_id with
  | Ok () -> Alcotest.fail "a mismatch"
  | Error m ->
      Alcotest.(check int) "record" 1 m.m_record;
      Alcotest.(check string) "key" "id 1" m.key;
      Alcotest.(check int) "stored" 20 m.stored.value;
      Alcotest.(check int) "recomputed" 50 m.recomputed.value;
      Alcotest.(check string)
        "shown"
        "after record 1, id 1 is stored as Newest { key: \"id 1\", time: 2, value: 20 }, \
         but every record so far gives Newest { key: \"id 1\", time: 5, value: 50 }"
        (Format.asprintf "%a" (pp_mismatch pp_newest) m)

let a_key_no_longer_maintained_is_skipped () =
  let skip_all = check_log ~recompute:(fun _ _ -> None) false two_of_one_id in
  Alcotest.(check bool) "skipped" true (skip_all = Ok ())

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "projection incremental"
    [
      ( "incremental",
        [
          t "a projection kept by event time equals its recomputation"
            a_projection_kept_by_event_time_equals_its_recomputation;
          t "a projection overwritten in arrival order is found and shrunk"
            a_projection_overwritten_in_arrival_order_is_found_and_shrunk;
          t "a mismatch names the record, the key and both outputs"
            a_mismatch_names_the_record_the_key_and_both_outputs;
          t "a key no longer maintained is skipped" a_key_no_longer_maintained_is_skipped;
        ] );
    ]
