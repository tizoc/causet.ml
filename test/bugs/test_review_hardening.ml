(* Review round 4: a rejected inbox closes its bus. Ported from the Rust
   port's tests/review_hardening.rs; the two tests about dropping an unused
   inbox and about root types cannot apply (no Drop; root types are checked
   by the OCaml compiler). *)

open Causet
open Bugs_common

let pushed () = Pushed { note = "late" }

let rejecting_a_foreign_bus_closes_its_inbox () =
  let first = protocol () in
  let second = protocol () in
  let a = Protocol.resolve_exn first (Path.child (Path.root app) a_slot) in
  let sink, inbox = Event_bus.create first ~worker_capacity:1 in
  (match
     Store.create second ~state:empty_app ~services:{ downloads = inline_service } inbox
   with
  | Error Foreign_bus -> ()
  | Error _ -> Alcotest.fail "another error"
  | Ok _ -> Alcotest.fail "the Store was made");
  match Sink.try_send_to sink a download_events (pushed ()) with
  | Error (Closed ev) -> Alcotest.(check bool) "the Event back" true (ev = pushed ())
  | Ok () -> Alcotest.fail "sent to a rejected inbox"
  | Error _ -> Alcotest.fail "another send error"

let () =
  Alcotest.run "review_hardening"
    [
      ( "review_hardening",
        [
          Alcotest.test_case "rejecting a foreign bus closes its inbox" `Quick
            rejecting_a_foreign_bus_closes_its_inbox;
        ] );
    ]
