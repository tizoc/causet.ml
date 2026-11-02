(* Port of the Rust tests/clock.rs.

   The execution clock: execution IDs count executions from 1, apart from
   time; execution times are [max previous now], so they never go back; root
   Actions are admitted at their ingress time, queued Actions at the time of
   the most recently executed item. *)

open Causet
open Common

type gate_state = {
  deadline : timestamp;  (** [Late] and [Ping] are admitted only before this time. *)
  ran : (string * timestamp) list;  (** Each executed Action's kind and [meta.time]. *)
}

type gate_action =
  | Start  (** Queues a [Late]. *)
  | Late
  | Ping
  | Panic_after of timestamp
      (** Its enabling check raises when admitted after this time. *)

let gate_actions =
  Message.make ~name:"gate.action"
    ~kind:(function
      | Start -> "gate.start"
      | Late -> "gate.late"
      | Ping -> "gate.ping"
      | Panic_after _ -> "gate.panic_after")
    ()

let gate : (gate_state, unit, gate_action, never, never) Component.t =
  Component.make ~name:"gate" ~actions:gate_actions ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun state action now ->
      match action with
      | Start -> true
      | Late | Ping -> now < state.deadline
      | Panic_after limit ->
          if not (now <= limit) then
            failwith (Printf.sprintf "admitted at %d, after %d" now limit);
          true)
    ~reduce:(fun ctx action (meta : Meta.t) ->
      Ctx.update ctx (fun s ->
          { s with ran = s.ran @ [ (gate_actions.kind action, meta.time) ] });
      match action with Start -> Ctx.emit_action ctx Late | _ -> ())
    ()

(* Moves [step] forward on every reading. The Store's first reading, in
   [Store.create], is its anchor. *)
let step_clock ~now ~step =
  let now = ref now in
  {
    Clock.now =
      (fun () ->
        now := !now + step;
        !now);
  }

let protocol () =
  let reg = Registry.create () in
  Registry.root reg gate;
  freeze_exn reg

let store state clock =
  let p = protocol () in
  let _sink, inbox = Event_bus.create p ~worker_capacity:4 in
  let store = Store.create_exn ~clock p ~state ~services:() inbox in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  (store, log, Protocol.resolve_exn p (Path.root gate))

let far () = { deadline = max_int; ran = [] }
let entries log = Observe.Trace_log.entries log
let pp_ran = pp_list (fun f (k, t) -> Format.fprintf f "(%S, %d)" k t)
let pp_pairs = pp_list (fun f (a, b) -> Format.fprintf f "(%d, %d)" a b)
let ok name r = check_dispatch name (Ok Executed) r

let ids_count_executions_and_a_standing_clock_gives_equal_times () =
  let s, log, gate = store (far ()) (Clock.Manual.clock (Clock.Manual.create 1000)) in
  ok "first" (Store.dispatch_to s gate Ping);
  ok "second" (Store.dispatch_to s gate Ping);
  eq ~pp:pp_pairs "(id, time)"
    [ (1, 1000); (2, 1000) ]
    (List.map (fun (e : Observe.Trace_entry.t) -> (e.id, e.time)) (entries log))

let times_follow_the_clock_forward_and_never_backwards () =
  let clock = Clock.Manual.create 1000 in
  let s, log, gate = store (far ()) (Clock.Manual.clock clock) in
  Clock.Manual.set clock 5000;
  ok "at 5000" (Store.dispatch_to s gate Ping);
  Clock.Manual.set clock 10;
  ok "at 10" (Store.dispatch_to s gate Ping);
  eq ~pp:pp_pairs "(id, time)"
    [ (1, 5000); (2, 5000) ]
    (List.map (fun (e : Observe.Trace_entry.t) -> (e.id, e.time)) (entries log));
  eq ~pp:pp_ran "ran" [ ("gate.ping", 5000); ("gate.ping", 5000) ] (Store.state s).ran

let a_root_action_is_admitted_at_its_ingress_time_and_a_refused_time_is_discarded () =
  let clock = Clock.Manual.create 1000 in
  let s, log, gate = store { deadline = 1500; ran = [] } (Clock.Manual.clock clock) in
  (* Admission reads the ingress time (1600), not the previous execution's
     time (1000): past the deadline, refused. *)
  Clock.Manual.set clock 1600;
  check_dispatch "refused" (Ok Disabled) (Store.dispatch_to s gate Ping);
  eq ~pp:Format.pp_print_int "no executions" 0 (Store.executions s);
  (* The refused reading took no ID and moved no time: the next execution is
     the first, at the clock's reading. *)
  Clock.Manual.set clock 1200;
  ok "admitted" (Store.dispatch_to s gate Ping);
  let first = List.hd (entries log) in
  eq ~pp:pp_pairs "(id, time)" [ (1, 1200) ] [ (first.id, first.time) ]

let a_queued_action_is_admitted_at_the_time_of_the_last_execution () =
  (* Readings: 1000 for the anchor, 1100 at Start's ingress, 1200 when Late
     starts. *)
  let s, _, gate = store { deadline = 1150; ran = [] } (step_clock ~now:900 ~step:100) in
  ok "start" (Store.dispatch_to s gate Start);
  (* Late was admitted at Start's time (1100 < 1150), not at the clock's
     next reading, and then took that reading as its own time. *)
  eq ~pp:pp_ran "ran" [ ("gate.start", 1100); ("gate.late", 1200) ] (Store.state s).ran

let a_clock_at_its_end_keeps_its_time_and_executions_go_on () =
  (* [Timestamp::MAX] in Rust is [u64::MAX]; here it is [max_int]. *)
  let s, log, gate = store (far ()) (Clock.Manual.clock (Clock.Manual.create max_int)) in
  ok "at the end" (Store.dispatch_to s gate (Panic_after max_int));
  check_dispatch "refused" (Ok Disabled) (Store.dispatch_to s gate Ping);
  ok "start" (Store.dispatch_to s gate Start);
  (* Ping, and the Late that Start queues, are refused: [now < deadline]
     cannot hold at the end of time. Neither takes an ID. *)
  eq ~pp:pp_pairs "(id, time)"
    [ (1, max_int); (2, max_int) ]
    (List.map (fun (e : Observe.Trace_entry.t) -> (e.id, e.time)) (entries log))

let the_system_clock_reads_the_real_time_and_its_offset () =
  let wall () = Int64.to_int (Int64.of_float (Unix.gettimeofday () *. 1e9)) in
  let second = 1_000_000_000 in
  let before = wall () in
  let reading = Clock.now (Clock.system ()) in
  if not (reading >= before && reading <= wall () + second) then
    Alcotest.failf "%d not near %d" reading before;
  (* The application corrects it as it learns the machine's error. *)
  let offset = Clock.Offset.create 0 in
  let clock = Clock.system ~offset () in
  Clock.Offset.set offset (3_600 * second);
  let ahead = Clock.now clock in
  if not (ahead >= before + (3_600 * second)) then
    Alcotest.fail "the offset applies to every reading"

let () =
  Alcotest.run "clock"
    [
      ( "clock",
        [
          Alcotest.test_case "ids count executions and a standing clock gives equal times"
            `Quick ids_count_executions_and_a_standing_clock_gives_equal_times;
          Alcotest.test_case "times follow the clock forward and never backwards" `Quick
            times_follow_the_clock_forward_and_never_backwards;
          Alcotest.test_case
            "a root action is admitted at its ingress time and a refused time is \
             discarded"
            `Quick
            a_root_action_is_admitted_at_its_ingress_time_and_a_refused_time_is_discarded;
          Alcotest.test_case
            "a queued action is admitted at the time of the last execution" `Quick
            a_queued_action_is_admitted_at_the_time_of_the_last_execution;
          Alcotest.test_case "a clock at its end keeps its time and executions go on"
            `Quick a_clock_at_its_end_keeps_its_time_and_executions_go_on;
          Alcotest.test_case "the system clock reads the real time and its offset" `Quick
            the_system_clock_reads_the_real_time_and_its_offset;
        ] );
    ]
