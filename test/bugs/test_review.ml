(* Regressions from the independent review of the prototype, plus a
   composite with no Effects carrying its child's Service. Ported from the
   Rust port's tests/review.rs. *)

open Causet
open Bugs_common

let late () = Pushed { note = "late" }

let send_string = function
  | Ok () -> "Ok"
  | Error (Full (Pushed { note })) -> "Full " ^ note
  | Error (Closed (Pushed { note })) -> "Closed " ^ note
  | Error (Wrong_protocol (Pushed { note })) -> "Wrong_protocol " ^ note
  | Error (Unregistered (Pushed { note })) -> "Unregistered " ^ note
  | Error _ -> "Error with another Event"

let check_sent what expected got =
  Alcotest.(check string) what (send_string expected) (send_string got)

let closed_store_lane_returns_the_event_on_its_own_thread () =
  let f = fixture inline_service 4 in
  ignore (Store.drain_events f.store 0) (* this thread now owns the Store lane *);
  let sink, a = (f.sink, f.a) in
  Store.close f.store;
  check_sent "closed"
    (Error (Closed (late ())))
    (Sink.try_send_to sink a download_events (late ()))

let closed_store_returns_the_event_on_another_thread () =
  let f = fixture inline_service 4 in
  let sink, a = (f.sink, f.a) in
  Store.close f.store;
  let result = ref (Ok ()) in
  Thread.join
    (Thread.create
       (fun () -> result := Sink.try_send_to sink a download_events (late ()))
       ());
  check_sent "closed" (Error (Closed (late ()))) !result

let event_sink_rejects_a_foreign_protocol_mount () =
  let foreign = fixture inline_service 4 in
  let local = fixture inline_service 4 in
  let event () = Pushed { note = "foreign mount" } in
  let sent = Sink.try_send_to local.sink foreign.a download_events (event ()) in
  check_sent "sent" (Error (Wrong_protocol (event ()))) sent;
  Alcotest.(check bool) "nothing drained" true (Store.drain_events local.store 1 = Ok 0);
  Alcotest.(check (list string)) "pushed" [] (Store.state local.store).a.pushed

let a_store_refuses_a_bus_made_for_another_protocol () =
  let first = protocol () in
  let second = protocol () in
  let _sink, inbox = Event_bus.create first ~worker_capacity:4 in
  match
    Store.create second ~state:empty_app ~services:{ downloads = inline_service } inbox
  with
  | Error Foreign_bus -> ()
  | Error _ -> Alcotest.fail "another error"
  | Ok _ -> Alcotest.fail "the Store was made"

(* A composite with no Effects whose child has some: it carries the child's
   Service through it. *)
type hub_state = { downloads : downloader_state }
type hub_services = { downloads_service : download_service }

let downloads_slot = Child.make "downloads" downloader

let hub =
  Component.make ~name:"hub" ~actions:noops ~effects:Message.never ~outputs:Message.never
    ~reduce:(fun _ Noop _ -> ())
    ~register:(fun r ->
      Registrar.child r downloads_slot
        ~lens:
          (Lens.field ~get:(fun s -> s.downloads) ~set:(fun _ downloads -> { downloads }))
        ~service:(fun svc -> svc.downloads_service)
        ())
    ()

let no_effects_with_a_service_carries_a_childs_service () =
  let reg = Registry.create () in
  Registry.root reg hub;
  let p =
    match Registry.freeze reg with
    | Ok p -> p
    | Error problems ->
        Alcotest.failf "the child's Service is reachable: %s"
          (String.concat "; " (List.map problem_message problems))
  in
  let _sink, inbox = Event_bus.create p ~worker_capacity:4 in
  let store =
    Store.create_exn p
      ~state:{ downloads = empty_downloader }
      ~services:{ downloads_service = inline_service }
      inbox
  in
  let downloads = Protocol.resolve_exn p (Path.child (Path.root hub) downloads_slot) in
  ignore (Store.dispatch_exn store downloads (Fetch "alpha"));
  ignore (Store.drain_events store 16);
  Alcotest.(check (list (pair string int)))
    "done"
    [ ("alpha", 5) ]
    (Store.state store).downloads.done_

(* A registrar is live only while its [register] runs: one kept for later
   raises, rather than add a child or an Event to a tree that is frozen and
   in use. *)
let a_registrar_kept_past_its_register_raises () =
  let kept = ref None in
  let keeper =
    Component.make ~name:"keeper" ~actions:noops ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun _ Noop _ -> ())
      ~register:(fun r -> kept := Some r)
      ()
  in
  let reg = Registry.create () in
  Registry.root reg keeper;
  let p = match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze" in
  let r = Option.get !kept in
  let raises what f =
    match f () with
    | () -> Alcotest.failf "%s: a kept registrar was used" what
    | exception Invalid_argument m ->
        Alcotest.(check string)
          what "causet: /'s registrar used after its register returned" m
  in
  raises "child" (fun () ->
      Registrar.child r downloads_slot
        ~lens:(Lens.field ~get:(fun _ -> empty_downloader) ~set:(fun s _ -> s))
        ());
  raises "event" (fun () -> Registrar.event r noops (fun _ _ -> ()));
  Alcotest.(check bool)
    "no child resolves" true
    (Result.is_error (Protocol.resolve p (Path.child (Path.root keeper) downloads_slot)))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "review"
    [
      ( "review",
        [
          t "a registrar kept past its register raises"
            a_registrar_kept_past_its_register_raises;
          t "closed store lane returns the event on its own thread"
            closed_store_lane_returns_the_event_on_its_own_thread;
          t "closed store returns the event on another thread"
            closed_store_returns_the_event_on_another_thread;
          t "event sink rejects a foreign protocol mount"
            event_sink_rejects_a_foreign_protocol_mount;
          t "a store refuses a bus made for another protocol"
            a_store_refuses_a_bus_made_for_another_protocol;
          t "no effects with a service carries a child's service"
            no_effects_with_a_service_carries_a_childs_service;
        ] );
    ]
