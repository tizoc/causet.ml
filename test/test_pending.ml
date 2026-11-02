(* Pending: the first of an answer and its deadline takes a request, and the
   second is stale (the Rust port's pending.rs tests). *)

open Causet

let request index = { Request_id.session = 1; execution = 1; index }
let check_opt = Alcotest.(check (option string))

let the_first_to_arrive_takes_the_request_and_the_second_is_stale () =
  let p = Pending.add (request 0) "charge" Pending.empty in
  match Pending.take (request 0) p with
  | None -> Alcotest.fail "not pending"
  | Some (value, p) ->
      Alcotest.(check string) "taken" "charge" value;
      (* The timer for it, or a duplicate answer. *)
      check_opt "stale" None (Option.map fst (Pending.take (request 0) p));
      check_opt "never waited on here" None (Option.map fst (Pending.take (request 9) p))

let expire_takes_what_is_due_in_deadline_order () =
  let p =
    Pending.empty
    |> Pending.add ~deadline:300 (request 0) "late"
    |> Pending.add ~deadline:100 (request 1) "first"
    |> Pending.add (request 2) "no deadline"
    |> Pending.add ~deadline:100 (request 3) "also first"
  in
  Alcotest.(check (option int)) "next" (Some 100) (Pending.next_deadline p);
  let none, p = Pending.expire 99 p in
  Alcotest.(check int) "nothing due" 0 (List.length none);
  let due, p = Pending.expire 200 p in
  Alcotest.(check (list string)) "due" [ "first"; "also first" ] (List.map snd due);
  Alcotest.(check bool)
    "in request order" true
    (List.map fst due = [ request 1; request 3 ]);
  Alcotest.(check (option int)) "next" (Some 300) (Pending.next_deadline p);
  Alcotest.(check int) "left" 2 (Pending.cardinal p);
  check_opt "an answer after its deadline is stale" None
    (Option.map fst (Pending.take (request 1) p))

let a_cancelled_request_is_stale_and_its_value_can_change_while_it_waits () =
  let p = Pending.add ~deadline:50 (request 0) 1 Pending.empty in
  let p = Pending.update (request 0) (( + ) 1) p in
  Alcotest.(check (option int)) "changed" (Some 2) (Pending.find_opt (request 0) p);
  Alcotest.(check (option int)) "deadline" (Some 50) (Pending.deadline (request 0) p);
  let p = Pending.add (request 1) 7 p |> Pending.retain (fun _ v -> v <> 7) in
  Alcotest.(check bool)
    "a request retain dropped is stale" false
    (Pending.mem (request 1) p);
  let p = Pending.cancel (request 0) p in
  Alcotest.(check bool) "empty" true (Pending.is_empty p);
  Alcotest.(check (option int)) "no deadline" None (Pending.next_deadline p)

let a_new_request_replaces_the_one_in_flight () =
  let p =
    Pending.add ~deadline:100 (request 0) "first search" Pending.empty
    |> Pending.clear
    |> Pending.add (request 1) "second search"
  in
  check_opt "the old answer is stale" None (Option.map fst (Pending.take (request 0) p));
  Alcotest.(check (option int)) "no deadline" None (Pending.next_deadline p);
  check_opt "taken" (Some "second search") (Option.map fst (Pending.take (request 1) p))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "pending"
    [
      ( "pending",
        [
          t "the first to arrive takes the request and the second is stale"
            the_first_to_arrive_takes_the_request_and_the_second_is_stale;
          t "expire takes what is due in deadline order"
            expire_takes_what_is_due_in_deadline_order;
          t "a cancelled request is stale and its value can change while it waits"
            a_cancelled_request_is_stale_and_its_value_can_change_while_it_waits;
          t "a new request replaces the one in flight"
            a_new_request_replaces_the_one_in_flight;
        ] );
    ]
