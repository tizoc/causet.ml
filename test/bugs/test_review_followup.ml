(* Regressions from the third independent review. Ported from the Rust
   port's tests/review_followup.rs. *)

open Causet
open Bugs_common

let external_unregistered_events_are_rejected_before_enqueueing () =
  let f = fixture inline_service 4 in
  (* The downloader never registers Ping. *)
  let sent = Sink.try_send_to f.sink f.a pings Ping in
  let drained = Store.drain_events f.store 1 in
  Alcotest.(check bool) "Unregistered Ping" true (sent = Error (Unregistered Ping));
  Alcotest.(check bool) "nothing was queued" true (drained = Ok 0)

let () =
  Alcotest.run "review_followup"
    [
      ( "review_followup",
        [
          Alcotest.test_case "external unregistered events are rejected before enqueueing"
            `Quick external_unregistered_events_are_rejected_before_enqueueing;
        ] );
    ]
