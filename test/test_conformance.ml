(* The ports agree: the guide's shop, run through the same scenarios as the
   Rust port's conformance example, makes the same executions in the same
   order, with the same identities, times, causes, ordinals and depths, the
   same refusals and the same final State. [conformance/shop.txt] is that
   example's output. *)

open Causet
open Causet_guide.Shop

let origin = function
  | None -> "-"
  | Some (o : Meta.origin) ->
      Printf.sprintf "%d/%016Lx/%d/%d" o.causal o.component o.emission_ordinal
        o.lowering_ordinal

let sent = function
  | Observe.Root -> "root"
  | Observe.From_effect at -> Printf.sprintf "effect %d" at
  | Observe.Queued o -> "queued " ^ origin (Some o)
  | Observe.From_inline at -> Printf.sprintf "inline %d" at

let by = function
  | Observe.Root_guard -> "root-guard"
  | Observe.Guard -> "guard"
  | Observe.Component -> "component"
  | Observe.Absent -> "absent"

let category = function
  | Meta.Action -> "Action"
  | Meta.Effect -> "Effect"
  | Meta.Event -> "Event"

let ints l = "[" ^ String.concat ", " (List.map string_of_int l) ^ "]"
let strings l = "[" ^ String.concat ", " (List.map (Printf.sprintf "%S") l) ^ "]"

let print_trace buf name shop log =
  let line fmt =
    Printf.ksprintf
      (fun s ->
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      fmt
  in
  line "scenario %s" name;
  List.iter
    (fun (e : Observe.Trace_entry.t) ->
      line "exec %d time %d %s %s %s causal %d origin %s answers %s depth %d" e.id e.time
        (category e.category) e.path e.kind e.causal (origin e.origin)
        (match e.answers with None -> "-" | Some a -> string_of_int a)
        e.depth)
    (Observe.Trace_log.entries log);
  List.iter
    (fun (r : Observe.Refusal_entry.t) ->
      line "refused %s %s %s by %s at %d" r.path r.kind (sent r.sent) (by r.by) r.time)
    (Observe.Trace_log.refusals log);
  List.iter
    (fun (i : Observe.Inline_entry.t) ->
      line "inline %s %s within %d" i.path i.kind i.within)
    (Observe.Trace_log.inline log);
  let s = Store.state shop.store in
  line "state paid %s declined %s pending %d orders %d cart %s %d" (ints s.payments.paid)
    (ints s.payments.declined)
    (Pending.cardinal s.payments.pending)
    s.orders (strings s.cart.items) s.cart.total

let open_traced () =
  let shop = open_ ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000)) () in
  let log = Observe.Trace_log.create () in
  Store.add_observer shop.store (Observe.Trace_log.observer log);
  (shop, log)

let add shop item price =
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item; price }))

let checkout shop = ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout)
let drain shop n = ignore (Store.drain_events shop.store n)

let traces () =
  let buf = Buffer.create 4096 in
  let shop, log = open_traced () in
  add shop "tea" 4;
  checkout shop;
  drain shop 16;
  print_trace buf "one-order" shop log;
  let shop, log = open_traced () in
  add shop "tea" 4;
  add shop "pot" 60;
  checkout shop;
  checkout shop;
  add shop "kettle" 150;
  checkout shop;
  drain shop 1;
  add shop "cup" 3;
  drain shop 16;
  print_trace buf "declined-and-refused" shop log;
  Buffer.contents buf

let read path = In_channel.with_open_text path In_channel.input_all

let the_shop_runs_as_in_the_rust_port () =
  let expected = String.split_on_char '\n' (read "conformance/shop.txt") in
  let got = String.split_on_char '\n' (traces ()) in
  Alcotest.(check (list string)) "trace" expected got

(* The Rust port's conformance_messages example, declared here: a kind given
   in full on a constructor whose name it does not tell ([Clear], [Cut]),
   names of more than one word, a nested message, and a record of one kind.
   [conformance/messages.txt] is that example's output. *)
type heater_action =
  | Warm of { max_secs : int; last_reading : int option } [@fields max_secs]
  | Cut [@kind "kettle.heater.off"]
[@@deriving message ~name:"kettle.heater" ~prefix:"kettle.heater"]

type kettle_action =
  | Switch_on of { target_celsius : int; keep_warm : bool; label : string } [@level Info]
      [@fields target_celsius, display label]
  | Clear [@kind "kettle.emptied"]
  | Heater of heater_action [@nested]
[@@deriving message ~name:"kettle.action"]

type dry_alarm = { dry_secs : int; recent_readings : int list }
[@@deriving message ~name:"kettle.alarm" ~kind:"kettle.dry" ~level:Warn ~fields:dry_secs]

let level_name : Message.level -> string = function
  | Trace -> "trace"
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let printed (m : 'a Message.t) v =
  let fields = List.map (fun (name, x) -> Printf.sprintf " %s=%s" name x) (m.fields v) in
  Printf.sprintf "%s %s %s %s |%s" m.name (m.kind v)
    (level_name (m.level v))
    (Debug.to_string m.pp v) (String.concat "" fields)

let messages_print_as_in_the_rust_port () =
  let expected =
    String.split_on_char '\n' (String.trim (read "conformance/messages.txt"))
  in
  let got =
    [
      printed kettle_action_message
        (Switch_on { target_celsius = 100; keep_warm = true; label = "green \"tea\"" });
      printed kettle_action_message Clear;
      printed kettle_action_message
        (Heater (Warm { max_secs = 90; last_reading = Some (-4) }));
      printed kettle_action_message (Heater (Warm { max_secs = 5; last_reading = None }));
      printed kettle_action_message (Heater Cut);
      printed dry_alarm_message { dry_secs = 3; recent_readings = [ 21; -4 ] };
    ]
  in
  Alcotest.(check (list string)) "printed" expected got

(* The Rust port's conformance_guards example: root guards, slot guards and
   their order, on the root-guard tests' tree (guards/guarded.ml).
   [conformance/guards.txt] is that example's output. *)
let guards_trace () =
  let open Guarded in
  let buf = Buffer.create 4096 in
  let line fmt =
    Printf.ksprintf
      (fun s ->
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      fmt
  in
  let reg = Registry.create () in
  Registry.root reg app;
  Registry.admit reg (mid_path ()) mid_may_forward;
  Registry.admit reg (mid_leaf ()) may_add;
  Registry.admit reg (other_leaf ()) other_may_add;
  Registry.admit reg (spare ()) spare_never;
  let proto =
    match Registry.freeze reg with
    | Ok p -> p
    | Error problems -> failwith (String.concat "; " (List.map problem_message problems))
  in
  let sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let state = { empty with blocked = [ 5 ]; other_blocked = [ 7 ]; limit = 1_000 } in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state ~services:() inbox
  in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let resolve path = Protocol.resolve_exn proto path in
  let app_m = resolve (Path.root app) in
  let mid_m = resolve (mid_path ()) in
  let other_m = resolve (Path.child (Path.root app) other_slot) in
  let leaf_m = resolve (mid_leaf ()) in
  let other_leaf_m = resolve (other_leaf ()) in
  let outcomes = ref [] in
  let dispatch mount action =
    outcomes := Store.dispatch_exn store mount action :: !outcomes
  in
  (* Every route into /mid/leaf, blocked and admitted. *)
  List.iter
    (fun n ->
      dispatch leaf_m (Add n);
      dispatch leaf_m (Again n);
      dispatch mid_m (Send (Add n));
      (match Sink.try_send_to sink leaf_m bumped_message (Bumped n) with
      | Ok () -> ()
      | Error _ -> failwith "the Event was not sent");
      ignore (Store.drain_events store 16);
      dispatch leaf_m (Ask n);
      dispatch leaf_m (Echo n))
    [ 5; 1 ];
  (* Forwarded: checked where it entered, by the root guard on /mid, from
     outside and on the root's own inline calls. *)
  dispatch mid_m (Forward (Add 5));
  List.iter (fun n -> dispatch app_m (Mid (Forward (Add n)))) [ 5; 2 ];
  (* The root's changes so far, seen by the root guard on its inline call. *)
  dispatch app_m (Block_then_forward 4);
  (* The root's inline calls check in the queued order: the root guard (131,
     blocked and ending in 31), the slot guard (1031, ending in 31 and 1000
     or more), the component's own check (1000), and availability before all
     of them (/spare). *)
  dispatch app_m (Block 131);
  List.iter (fun n -> dispatch app_m (Mid (Forward (Add n)))) [ 131; 1031; 1000 ];
  dispatch app_m (Spare (Forward (Add 5)));
  (* Inline into /other/leaf, from /other: its root guard does not run; the
     slot guard and the leaf's own check do. *)
  List.iter (fun n -> dispatch other_m (Forward (Add n))) [ 7; 13; 100 ];
  (* The other mount's own root guard. *)
  dispatch other_leaf_m (Add 7);
  (* Order: the slot guard before the leaf's own check (113), the root
     guard before both (150, 13, once blocked). *)
  List.iter (fun n -> dispatch app_m (Block n)) [ 150; 13 ];
  List.iter (fun n -> dispatch leaf_m (Add n)) [ 113; 150; 13 ];
  let total (m : mid_state) =
    match m.leaf with None -> "-" | Some l -> string_of_int l.total
  in
  let totals =
    Printf.sprintf "mid %s other %s"
      (total (Store.state store).mid)
      (total (Store.state store).other)
  in
  (* Absent first, from outside, queued and inline. *)
  dispatch mid_m Drop_leaf;
  dispatch leaf_m (Add 13);
  dispatch mid_m (Send (Add 113));
  dispatch other_m Drop_leaf;
  dispatch other_m (Forward (Add 113));
  line "scenario root-guards";
  line "outcomes %s"
    (String.concat " "
       (List.rev_map
          (function
            | Executed -> "executed" | Disabled -> "disabled" | Faulted -> "faulted")
          !outcomes));
  List.iter
    (fun (e : Observe.Trace_entry.t) ->
      line "exec %d time %d %s %s %s causal %d origin %s answers %s depth %d" e.id e.time
        (category e.category) e.path e.kind e.causal (origin e.origin)
        (match e.answers with None -> "-" | Some a -> string_of_int a)
        e.depth)
    (Observe.Trace_log.entries log);
  List.iter
    (fun (r : Observe.Refusal_entry.t) ->
      line "refused %s %s %s by %s at %d" r.path r.kind (sent r.sent) (by r.by) r.time)
    (Observe.Trace_log.refusals log);
  List.iter
    (fun (i : Observe.Inline_entry.t) ->
      line "inline %s %s within %d%s" i.path i.kind i.within
        (if i.root_guard_skipped then " root-guard-skipped" else ""))
    (Observe.Trace_log.inline log);
  let s = Store.state store in
  let calls (m : mid_state) =
    match m.outcomes with
    | [] -> "-"
    | l ->
        String.concat " "
          (List.map (function Reduced -> "reduced" | Refused -> "refused") l)
  in
  line "totals before the drops %s" totals;
  line "state mid %s other %s" (total s.mid) (total s.other);
  line "calls mid %s" (calls s.mid);
  line "calls other %s" (calls s.other);
  Buffer.contents buf

let root_guards_run_as_in_the_rust_port () =
  let expected = String.split_on_char '\n' (read "conformance/guards.txt") in
  let got = String.split_on_char '\n' (guards_trace ()) in
  Alcotest.(check (list string)) "trace" expected got

let () =
  Alcotest.run "conformance"
    [
      ( "shop",
        [
          Alcotest.test_case "the shop runs as in the Rust port" `Quick
            the_shop_runs_as_in_the_rust_port;
        ] );
      ( "messages",
        [
          Alcotest.test_case "messages print as in the Rust port" `Quick
            messages_print_as_in_the_rust_port;
        ] );
      ( "guards",
        [
          Alcotest.test_case "root guards run as in the Rust port" `Quick
            root_guards_run_as_in_the_rust_port;
        ] );
    ]
