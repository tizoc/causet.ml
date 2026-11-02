(* The downloads scenario ([Downloads]): Effects, a shared Service, and
   replies as Events through [drain_events] (the Rust port's
   tests/services.rs). *)

open Causet
open Downloads

let entries f = Observe.Trace_log.entries f.trace

let request_round_trip_through_a_shared_threaded_service () =
  let f = fixture (threaded ~duplicate:false) 16 in
  ok_dispatch f.store f.a (Fetch "alpha");
  ok_dispatch f.store f.b (Fetch "bravo-long");
  drain_until f.store (fun s -> s.a.done_ <> [] && s.b.done_ <> []);

  let s = Store.state f.store in
  Alcotest.(check (list (pair string int))) "a done" [ ("alpha", 5) ] s.a.done_;
  Alcotest.(check (list (pair string int))) "b done" [ ("bravo-long", 10) ] s.b.done_;
  Alcotest.(check bool) "nothing pending" true (s.a.pending = [] && s.b.pending = []);
  Alcotest.(check bool) "nothing stale" true (s.a.stale = [] && s.b.stale = []);

  (* Each reply is a root Event execution at the mount that asked, and its
     route names the Effect that issued it. *)
  let trace = entries f in
  List.iter
    (fun path ->
      let find category what =
        match
          List.find_opt
            (fun (e : Observe.Trace_entry.t) -> e.category = category && e.path = path)
            trace
        with
        | Some e -> e
        | None -> Alcotest.failf "%s: the %s" path what
      in
      let eff = find Meta.Effect "fetch Effect" in
      let event = find Meta.Event "reply Event" in
      Alcotest.(check (option int)) (path ^ " answers") (Some eff.id) event.answers;
      Alcotest.(check int) (path ^ ": an Event is a root input") event.id event.causal)
    [ "/a"; "/b" ];
  Store.close f.store

let duplicate_reply_is_ignored_as_stale () =
  let f = fixture (threaded ~duplicate:true) 16 in
  ok_dispatch f.store f.a (Fetch "alpha");
  drain_until f.store (fun s -> List.length s.a.done_ = 1 && List.length s.a.stale = 1);
  let s = Store.state f.store in
  Alcotest.(check int) "done" 1 (List.length s.a.done_);
  Alcotest.(check int) "stale" 1 (List.length s.a.stale);
  Store.close f.store

let store_lane_is_unbounded_and_drained_before_the_worker_lane () =
  let f = fixture inline 1 in
  (* Replies sent on the Store's thread: three, with a worker capacity of 1. *)
  List.iter (fun url -> ok_dispatch f.store f.a (Fetch url)) [ "one"; "two"; "three" ];
  (* A worker thread fills the bounded lane and gets its Event back. *)
  let sink, a = (f.sink, f.a) in
  let first, second =
    in_thread (fun () ->
        let first = Sink.try_send_to sink a download_events (Pushed { note = "w1" }) in
        let second = Sink.try_send_to sink a download_events (Pushed { note = "w2" }) in
        (first, second))
  in
  Alcotest.check download_send_result "first" (Ok ()) first;
  Alcotest.check download_send_result "second refused"
    (Error (Full (Pushed { note = "w2" })))
    second;

  Alcotest.check drained "four drained" (Ok 4) (Store.drain_events f.store 16);
  let kinds =
    List.filter_map
      (fun (e : Observe.Trace_entry.t) ->
        if e.category = Meta.Event then Some e.kind else None)
      (entries f)
  in
  Alcotest.(check (list string))
    "Store lane first"
    [ "download.done"; "download.done"; "download.done"; "download.pushed" ]
    kinds;
  Alcotest.(check int) "done" 3 (List.length (Store.state f.store).a.done_);
  Alcotest.(check (list string)) "pushed" [ "w1" ] (Store.state f.store).a.pushed;
  Store.close f.store

let category_name c = Meta.category_name c

let refused_synchronous_child_leaves_no_ordinal_gap () =
  let f = fixture inline 4 in
  ok_dispatch f.store f.a Probe;
  let trace = entries f in
  Alcotest.(check (list (pair string string)))
    "trace"
    [
      ("action", "downloader.probe");
      ("effect", "downloader.effect.probe");
      ("action", "downloader.note");
    ]
    (List.map
       (fun (e : Observe.Trace_entry.t) -> (category_name e.category, e.kind))
       trace);
  let eff = List.nth trace 1 and note = List.nth trace 2 in
  (match note.origin with
  | None -> Alcotest.fail "an Effect child has an origin"
  | Some o ->
      Alcotest.(check int) "caused by the Effect" eff.id o.causal;
      Alcotest.(check int) "the refused attempt claimed nothing" 0 o.emission_ordinal);
  Alcotest.(check (list int)) "notes" [ 1 ] (Store.state f.store).a.notes

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let reply_for_an_unregistered_event_faults () =
  let f = fixture inline 4 in
  match Store.dispatch_to f.store f.a Bad_reply with
  | Error (Faulted message) ->
      if not (contains message "/a never registered Event ping") then
        Alcotest.failf "message: %s" message
  | r -> Alcotest.failf "expected a fault, got %a" (Alcotest.pp dispatched) r

let leaf_without_effects_needs_no_service_binding () =
  let f = fixture inline 4 in
  ok_dispatch f.store f.counter Inc;
  Alcotest.(check int) "n" 1 (Store.state f.store).counter.n

(* A root whose effectful child is mounted with no Service projection. *)
type unwired_parent = app_state * app_services * noop * never * never

let unwired_slot :
    ( unwired_parent,
      downloader_state,
      download_service,
      downloader_action,
      downloader_effect,
      never )
    Child.t =
  Child.make "a" downloader

let effectful_child_without_a_service_projection_fails_freeze () =
  let unwired : (app_state, app_services, noop, never, never) component =
    Component.make ~name:"unwired" ~actions:noops ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun _ Noop _ -> ())
      ~register:(fun r ->
        Registrar.child r unwired_slot
          ~lens:
            (Lens.field
               ~get:(fun (s : app_state) -> s.a)
               ~set:(fun (s : app_state) a -> { s with a }))
          ())
      ()
  in
  let reg = Registry.create () in
  Registry.root reg unwired;
  match Registry.freeze reg with
  | Ok _ -> Alcotest.fail "must fail"
  | Error problems ->
      if not (List.mem (No_service_projection { path = "/a"; at = "/a" }) problems) then
        Alcotest.failf "problems: %s"
          (String.concat "; " (List.map problem_message problems))

let closed_bus_hands_the_event_back () =
  let f = fixture inline 4 in
  let sink, a = (f.sink, f.a) in
  (* Rust: drop(f). *)
  Store.close f.store;
  Alcotest.check download_send_result "handed back"
    (Error (Closed (Pushed { note = "late" })))
    (in_thread (fun () ->
         Sink.try_send_to sink a download_events (Pushed { note = "late" })))

let () =
  Alcotest.run "events.services"
    [
      ( "services",
        [
          Alcotest.test_case "request round trip through a shared threaded Service" `Quick
            request_round_trip_through_a_shared_threaded_service;
          Alcotest.test_case "a duplicate reply is ignored as stale" `Quick
            duplicate_reply_is_ignored_as_stale;
          Alcotest.test_case
            "the Store lane is unbounded and drained before the worker lane" `Quick
            store_lane_is_unbounded_and_drained_before_the_worker_lane;
          Alcotest.test_case "a refused synchronous child leaves no ordinal gap" `Quick
            refused_synchronous_child_leaves_no_ordinal_gap;
          Alcotest.test_case "a reply for an unregistered Event faults" `Quick
            reply_for_an_unregistered_event_faults;
          Alcotest.test_case "a leaf without Effects needs no Service binding" `Quick
            leaf_without_effects_needs_no_service_binding;
          Alcotest.test_case
            "an effectful child without a Service projection fails freeze" `Quick
            effectful_child_without_a_service_projection_fails_freeze;
          Alcotest.test_case "a closed bus hands the Event back" `Quick
            closed_bus_hands_the_event_back;
        ] );
    ]
