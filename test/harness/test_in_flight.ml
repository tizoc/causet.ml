(* Port of causet-test's tests/in_flight.rs: [In_flight], a simulated world's
   messages in flight. Rust's end of time, u64::MAX, is max_int here. *)

open Causet_test
open Harness_util

let strings = Alcotest.(list string)

let in_flight_delivers_by_time_then_by_sending () =
  let flight : (string, unit) In_flight.t = In_flight.create () in
  Alcotest.(check (option int)) "empty" None (In_flight.next_time flight);
  In_flight.send_at flight 20 "c";
  In_flight.send_at flight 10 "a";
  In_flight.send_at flight 10 "b";
  In_flight.send_at flight 30 "d";
  Alcotest.(check (option int)) "next" (Some 10) (In_flight.next_time flight);
  Alcotest.check strings "due 9" [] (In_flight.due flight 9);
  Alcotest.check strings "due 20" [ "a"; "b"; "c" ] (In_flight.due flight 20);
  Alcotest.(check int) "len" 1 (In_flight.length flight);
  Alcotest.check strings "due at the end" [ "d" ] (In_flight.due flight max_int);
  Alcotest.(check bool) "empty" true (In_flight.is_empty flight)

let in_flight_keeps_a_channel_in_order_whatever_the_delays () =
  let flight : (string, int) In_flight.t = In_flight.create () in
  Alcotest.(check int) "first" 50 (In_flight.send_in_order flight 1 50 "first");
  (* Drawn earlier, it still comes after the channel's last message. *)
  Alcotest.(check int) "second" 51 (In_flight.send_in_order flight 1 10 "second");
  (* Another channel, and a message on none, are not held back. *)
  Alcotest.(check int) "other" 10 (In_flight.send_in_order flight 2 10 "other");
  In_flight.send_at flight 20 "unordered";
  Alcotest.check strings "due 51"
    [ "other"; "unordered"; "first"; "second" ]
    (In_flight.due flight 51)

(* Regression: after a message due at the end of time, the next on its
   channel overflowed [last + 1]. *)
let a_channel_at_the_end_of_time_keeps_its_order () =
  let flight : (string, int) In_flight.t = In_flight.create () in
  Alcotest.(check int) "first" max_int (In_flight.send_in_order flight 1 max_int "first");
  Alcotest.(check int) "second" max_int (In_flight.send_in_order flight 1 0 "second");
  Alcotest.(check int) "third" max_int (In_flight.send_in_order flight 1 7 "third");
  Alcotest.check strings "due"
    [ "first"; "second"; "third" ]
    (In_flight.due flight max_int)

(* Regression: a channel's first message was due one after the time asked,
   as if a message had gone before it at zero. *)
let a_channel_s_first_message_is_due_when_asked () =
  let flight : (string, int) In_flight.t = In_flight.create () in
  Alcotest.(check int) "first" 0 (In_flight.send_in_order flight 1 0 "first");
  Alcotest.(check int) "second" 1 (In_flight.send_in_order flight 1 0 "second");
  Alcotest.(check int) "other" 0 (In_flight.send_in_order flight 2 0 "other");
  Alcotest.check strings "due 0" [ "first"; "other" ] (In_flight.due flight 0)

(* Rust also checks the Debug form shows the messages; In_flight has no
   printer in the OCaml harness, so only the listing is checked. *)
let in_flight_shows_what_it_holds_in_order () =
  let flight : (string, unit) In_flight.t = In_flight.create () in
  In_flight.send_at flight 5 "b";
  In_flight.send_at flight 3 "a";
  Alcotest.(check (list (pair int string)))
    "held"
    [ (3, "a"); (5, "b") ]
    (In_flight.to_list flight)

(* Generated sends on three channels and on none, and deliveries, at times
   drawn near both ends of time and between. Checked: each delivery is every
   message due by then, in order of when it is due and then of sending; a
   message off a channel is due when asked; on a channel, never before it was
   asked, its first when asked, one asked after the last message's time when
   asked, and in the order sent. *)
let generated rng =
  let time rng =
    match Rng.below rng 4 with
    | 0 -> Rng.below rng 4
    | 1 -> max_int - Rng.below rng 4
    | _ -> Rng.below rng 1_000
  in
  let flight : (int, int) In_flight.t = In_flight.create () in
  (* By message: when it is due. *)
  let due_at = Dynarray.create () in
  let last = Hashtbl.create 3 in
  let pending = ref [] in
  let by_due a b = compare (Dynarray.get due_at a, a) (Dynarray.get due_at b, b) in
  while Rng.more rng do
    let at = time rng in
    let sent =
      match Rng.below rng 3 with
      | 0 ->
          In_flight.send_at flight at (Dynarray.length due_at);
          Dynarray.add_last due_at at;
          true
      | 1 ->
          let channel = Rng.below rng 3 in
          let due = In_flight.send_in_order flight channel at (Dynarray.length due_at) in
          if due < at then Alcotest.failf "due at %d, asked for %d" due at;
          (match Hashtbl.find_opt last channel with
          | None -> Alcotest.(check int) "a channel's first message" at due
          | Some previous ->
              if due < previous then Alcotest.failf "due at %d before %d" due previous;
              if at > previous then
                Alcotest.(check int)
                  (Printf.sprintf "asked after the last, at %d" previous)
                  at due);
          Hashtbl.replace last channel due;
          Dynarray.add_last due_at due;
          true
      | _ ->
          let expected =
            List.filter (fun m -> Dynarray.get due_at m <= at) !pending
            |> List.sort by_due
          in
          Alcotest.check ints
            (Printf.sprintf "due at %d" at)
            expected (In_flight.due flight at);
          pending := List.filter (fun m -> Dynarray.get due_at m > at) !pending;
          false
    in
    if sent then (
      pending := !pending @ [ Dynarray.length due_at - 1 ];
      let min_due =
        List.fold_left
          (fun acc m ->
            let d = Dynarray.get due_at m in
            match acc with None -> Some d | Some a -> Some (min a d))
          None !pending
      in
      Alcotest.(check (option int)) "next time" min_due (In_flight.next_time flight))
  done;
  let rest = List.sort by_due !pending in
  Alcotest.check ints "rest" rest (In_flight.due flight max_int);
  Alcotest.(check bool) "empty" true (In_flight.is_empty flight)

let generated_in_flight_delivers_in_order () = check ~config:default generated

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "in_flight"
    [
      ( "in_flight",
        [
          t "in flight delivers by time then by sending"
            in_flight_delivers_by_time_then_by_sending;
          t "in flight keeps a channel in order whatever the delays"
            in_flight_keeps_a_channel_in_order_whatever_the_delays;
          t "a channel at the end of time keeps its order"
            a_channel_at_the_end_of_time_keeps_its_order;
          t "a channel's first message is due when asked"
            a_channel_s_first_message_is_due_when_asked;
          t "in flight shows what it holds in order"
            in_flight_shows_what_it_holds_in_order;
          t "generated in flight delivers in order" generated_in_flight_delivers_in_order;
        ] );
    ]
