(* Ported from the Rust port's [tests/observers.rs]: observers on the
   downloads scenario: payload views at start only, Events and synchronous
   children in the lifecycle, faults and bus depths. *)

open Causet
open Common

(* Every callback as one line, payloads in their Debug form. *)
let calls_observer (lines : string list ref) =
  let push s = lines := !lines @ [ s ] in
  {
    Observe.none with
    action_start =
      (fun (exec : Observe.Execution.t) payload ->
        push
          (Format.asprintf "action_start %s %s %a" exec.path exec.kind Message.pp_payload
             payload));
    action_finish =
      (fun (exec : Observe.Execution.t) -> push ("action_finish " ^ exec.kind));
    action_settled =
      (fun (exec : Observe.Execution.t) -> push ("action_settled " ^ exec.kind));
    effect_start =
      (fun (exec : Observe.Execution.t) payload ->
        push
          (Format.asprintf "effect_start %s %s %a" exec.path exec.kind Message.pp_payload
             payload));
    effect_finish =
      (fun (exec : Observe.Execution.t) -> push ("effect_finish " ^ exec.kind));
    event_start =
      (fun (exec : Observe.Execution.t) payload ->
        push
          (Format.asprintf "event_start %s %s answers=%b %a" exec.path exec.kind
             (exec.answers <> None) Message.pp_payload payload));
    event_finish =
      (fun (exec : Observe.Execution.t) -> push ("event_finish " ^ exec.kind));
    event_settled =
      (fun (exec : Observe.Execution.t) -> push ("event_settled " ^ exec.kind));
    fault =
      (fun (fault : Observe.Fault_info.t) ->
        push
          (Printf.sprintf "fault %s at=%b %s" (phase_debug fault.phase) (fault.at <> None)
             fault.message));
    bus =
      (fun (d : Observe.Bus_depths.t) ->
        push (Printf.sprintf "bus %d %d %d" d.store_lane d.worker_lane d.rejected_full));
  }

let with_calls f =
  let lines = ref [] in
  Store.add_observer f.store (calls_observer lines);
  lines

(* Replaces each [RequestId { .. }], whose IDs are clock times, with [R]. *)
let redact line =
  let out = Buffer.create (String.length line) in
  let needle = "RequestId {" in
  let n = String.length needle in
  let rec find_from s i =
    if i + n > String.length s then None
    else if String.sub s i n = needle then Some i
    else find_from s (i + 1)
  in
  let rec go i =
    match find_from line i with
    | None -> Buffer.add_string out (String.sub line i (String.length line - i))
    | Some start ->
        Buffer.add_string out (String.sub line i (start - i));
        Buffer.add_char out 'R';
        let close = String.index_from line start '}' in
        go (close + 1)
  in
  go 0;
  Buffer.contents out

let a_request_and_its_reply_reach_observers_in_lifecycle_order () =
  let f = fixture inline_service 4 in
  let calls = with_calls f in
  ignore (Store.dispatch_exn f.store f.a (Fetch "alpha"));
  ignore (Store.drain_events f.store 16);
  Alcotest.(check (list string))
    "calls"
    [
      {|action_start /a downloader.fetch Fetch("alpha")|};
      "action_finish downloader.fetch";
      {|effect_start /a downloader.effect.fetch Fetch { request: R, url: "alpha" }|};
      "effect_finish downloader.effect.fetch";
      "action_settled downloader.fetch";
      (* The inline service replied on the Store lane. *)
      "bus 1 0 0";
      "event_start /a download.done answers=true Done { request: R, bytes: 5 }";
      "event_finish download.done";
      "action_start /a downloader.completed Completed { request: R, bytes: 5 }";
      "action_finish downloader.completed";
      {|action_start /a downloader.finished Finished { url: "alpha", bytes: 5 }|};
      "action_finish downloader.finished";
      "action_settled downloader.finished";
      "action_settled downloader.completed";
      "event_settled download.done";
    ]
    (List.map redact !calls)

let synchronous_effect_children_run_inside_their_effect () =
  let f = fixture inline_service 4 in
  let calls = with_calls f in
  ignore (Store.dispatch_exn f.store f.a Probe);
  (* Note 0 is refused: no callback. Note 1 runs whole before the Effect
     finishes. *)
  Alcotest.(check (list string))
    "calls"
    [
      "action_start /a downloader.probe Probe";
      "action_finish downloader.probe";
      "effect_start /a downloader.effect.probe Probe";
      "action_start /a downloader.note Note(1)";
      "action_finish downloader.note";
      "action_settled downloader.note";
      "effect_finish downloader.effect.probe";
      "action_settled downloader.probe";
    ]
    !calls

let a_fault_reaches_observers_with_its_phase_and_reason () =
  let f = fixture inline_service 4 in
  let calls = with_calls f in
  Alcotest.(check bool)
    "crash faults" true
    (Result.is_error (Store.dispatch_to f.store f.a Crash));
  Alcotest.(check (list string))
    "crash"
    [
      "action_start /a downloader.crash Crash"; "fault Reducer at=true downloader crashed";
    ]
    !calls;
  (* An admission raise belongs to no execution. *)
  let f = fixture inline_service 4 in
  let calls = with_calls f in
  Alcotest.(check bool)
    "explode faults" true
    (Result.is_error (Store.dispatch_to f.store f.a Explode));
  Alcotest.(check (list string))
    "explode"
    [ "fault Admission at=false admission exploded" ]
    !calls

let a_drain_reports_lane_depths_and_full_rejections () =
  let f = fixture inline_service 1 in
  let calls = with_calls f in
  let pushed note = Pushed { note } in
  let first = ref None and second = ref None in
  let worker =
    Thread.create
      (fun () ->
        first := Some (Sink.try_send_to f.sink f.a download_events (pushed "first"));
        second := Some (Sink.try_send_to f.sink f.a download_events (pushed "second")))
      ()
  in
  Thread.join worker;
  Alcotest.(check bool) "first sent" true (!first = Some (Ok ()));
  Alcotest.(check bool)
    "second is Full" true
    (!second = Some (Error (Full (pushed "second"))));
  Alcotest.(check bool) "one drained" true (Store.drain_events f.store 16 = Ok 1);
  Alcotest.(check bool) "none drained" true (Store.drain_events f.store 16 = Ok 0);
  let bus = List.filter (fun c -> String.starts_with ~prefix:"bus" c) !calls in
  (* Depths are taken before draining; the count resets once reported. *)
  Alcotest.(check (list string)) "bus" [ "bus 0 1 1"; "bus 0 0 0" ] bus

let a_panicking_observer_faults_the_store () =
  let f = fixture inline_service 4 in
  Store.add_observer f.store
    { Observe.none with action_finish = (fun _ -> failwith "observer broke") };
  let second_log = Observe.Trace_log.create () in
  Store.add_observer f.store (Observe.Trace_log.observer second_log);
  (match Store.dispatch_to f.store f.counter Inc with
  | Error (Faulted m) -> Alcotest.(check string) "message" "observer broke" m
  | _ -> Alcotest.fail "the Store should fault");
  (* It ran inside the root input: the Counter's Action had started. *)
  Alcotest.(check int) "one entry" 1 (List.length (Observe.Trace_log.entries f.log))

(* An observer reads a payload as its own type: the Actions it knows, read
   exactly, and nothing for the others. *)
let an_observer_reads_a_payload_as_its_type () =
  let open Causet_guide.Shop in
  let prices = ref [] in
  let shop = open_ () in
  Store.add_observer shop.store
    {
      Observe.none with
      action_start =
        (fun _ payload ->
          (match Message.get cart_action_message payload with
          | Some (Add { price; _ }) -> prices := !prices @ [ price ]
          | _ -> ());
          (* Another message type is not a cart action. *)
          if Message.get payments_action_message payload <> None then
            assert (Message.get cart_action_message payload = None));
    };
  List.iter
    (fun price ->
      ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price })))
    [ 3; 4 ];
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  Alcotest.(check (list int)) "prices" [ 3; 4 ] !prices

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "observers"
    [
      ( "observers",
        [
          t "a request and its reply reach observers in lifecycle order"
            a_request_and_its_reply_reach_observers_in_lifecycle_order;
          t "synchronous effect children run inside their effect"
            synchronous_effect_children_run_inside_their_effect;
          t "a fault reaches observers with its phase and reason"
            a_fault_reaches_observers_with_its_phase_and_reason;
          t "a drain reports lane depths and full rejections"
            a_drain_reports_lane_depths_and_full_rejections;
          t "a panicking observer faults the store" a_panicking_observer_faults_the_store;
          t "an observer reads a payload as its type"
            an_observer_reads_a_payload_as_its_type;
        ] );
    ]
