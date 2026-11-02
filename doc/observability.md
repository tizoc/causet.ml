The causet guide's OCaml examples for its observability page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: custom-observer -->
```ocaml
open Causet
open Causet_guide.Shop

(* One line per execution: its ID, the execution that caused it, its kind
   and where it ran. *)
let causes () =
  let lines = ref [] in
  let line (e : Observe.Execution.t) =
    let cause = Meta.causal_execution_id e.meta in
    lines := Printf.sprintf "%d <- %d %s %s" e.meta.id cause e.kind e.path :: !lines
  in
  ( {
      Observe.none with
      action_start = (fun e _ -> line e);
      effect_start = (fun e _ -> line e);
      event_start = (fun e _ -> line e);
    },
    fun () -> List.rev !lines )

let () =
  let shop = open_ () in
  let observer, lines = causes () in
  Store.add_observer shop.store observer;
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  assert (
    lines ()
    = [
        "1 <- 1 cart.add /cart";
        "2 <- 2 cart.checkout /cart";
        "3 <- 2 shop.pay /";
        "4 <- 3 payments.charge /payments";
        "5 <- 4 payments.charge /payments";
        "6 <- 6 payments.charged /payments";
        "7 <- 6 payments.settled /payments";
      ])
```

<!-- snippet: why-chain -->
```ocaml
let () =
  let shop = open_ () in
  let log = Observe.Trace_log.create () in
  Store.add_observer shop.store (Observe.Trace_log.observer log);
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  (* Why was the charge settled? Walk back from the last execution: an Event
     to the Effect it answers, anything else to its cause, up to a root input. *)
  let entries = Observe.Trace_log.entries log in
  let find id = List.find (fun (e : Observe.Trace_entry.t) -> e.id = id) entries in
  let rec why (step : Observe.Trace_entry.t) =
    let back = Option.value step.answers ~default:step.causal in
    step.kind :: (if back = step.id then [] (* a root input *) else why (find back))
  in
  assert (
    why (List.nth entries (List.length entries - 1))
    = [
        "payments.settled"; "payments.charged"; "payments.charge"; "payments.charge";
        "shop.pay"; "cart.checkout";
      ])
```

<!-- snippet: log-attributes -->
<!-- $MDX file=examples/observability_withdrawal.ml -->
```ocaml
type withdrawal_action =
  | Withdraw of { account : string; amount : int } [@level Info] [@fields amount]
      (** Logged at info, with its amount only: the account stays out of the logs. *)
  | Cancel
[@@deriving message ~name:"withdrawal.action" ~level:Debug]
```

<!-- snippet: logger-and-stats -->
```ocaml
let () =
  let open Causet_observers in
  let shop = open_ () in
  (* One Logs message per execution. *)
  Store.add_observer shop.store (logger ());
  (* Counts and durations, by kind. *)
  let stats = Stats.create () in
  Store.add_observer shop.store (Stats.observer stats);
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  let s = Stats.snapshot stats in
  let calls category ty kind = Duration_ranges.calls (List.assoc kind (List.assoc ty category)) in
  assert (calls s.actions "payments.action" "payments.charge" = 1);
  assert (calls s.events "payments.charged" "payments.charged" = 1)
```

<!-- snippet: invariant-orders -->
```ocaml
let () =
  let shop = open_ () in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer shop.store (Observe.Bug_log.observer bugs);
  (* Every order the shop took is paid, declined or still pending. *)
  Store.add_invariant ~pos:__POS__ shop.store "every order is accounted for"
    (fun state _ ->
      let p = state.payments in
      let charges =
        List.length p.paid + List.length p.declined + Pending.cardinal p.pending
      in
      if charges = state.orders then Ok ()
      else Error (Printf.sprintf "orders %d, charges %d" state.orders charges));
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  assert (Observe.Bug_log.reports bugs = []);
  (* A charge sent straight to payments, with no order behind it. *)
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 9 }));
  (match Observe.Bug_log.reports bugs with
  | [ { Observe.Bug_report.message; place = Some { phase; at; _ }; _ } ] ->
      assert (message = "orders 1, charges 2");
      assert (phase = Observe.Invariant "every order is accounted for");
      assert (at = Some 8) (* the root input that broke it *)
  | _ -> assert false);
  (* The Store goes on, and the invariant is checked again after each root input. *)
  ignore (Store.drain_events shop.store 16);
  assert (List.length (Observe.Bug_log.reports bugs) = 2)
```

<!-- snippet: invariant-memory -->
```ocaml
(* No paid charge is ever forgotten. The checker remembers how many it has
   seen, and reports each loss once, saying when it saw it. *)
let never_forgets () =
  let seen = ref 0 in
  fun (state : shop_state) (step : invariant_step) ->
    let paid = List.length state.payments.paid in
    let before = !seen in
    seen := paid;
    if paid >= before then Ok ()
    else
      let when_ =
        match step with
        | Added -> "when added"
        | Restored -> "on restore"
        | Settled_step { kind; _ } -> "after " ^ kind
      in
      Error (Printf.sprintf "%d paid, then %d %s" before paid when_)

let buy shop price =
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16)

let () =
  let shop = open_ () in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer shop.store (Observe.Bug_log.observer bugs);
  Store.add_invariant ~pos:__POS__ shop.store "paid charges are kept" (never_forgets ());
  buy shop 4;
  buy shop 6;
  assert (Observe.Bug_log.reports bugs = [])
```

<!-- snippet: invariant-readd -->
```ocaml
let () =
  let shop = open_ () in
  let kept = never_forgets () in
  Store.add_invariant ~pos:__POS__ shop.store "paid charges are kept" kept;
  buy shop 4;
  let stale = Store.state shop.store in
  buy shop 6;
  Store.close shop.store;
  (* The Store is replaced, and its successor starts from a stale snapshot,
     taken before the second charge was paid. The same checker, added
     again, still remembers. *)
  let services = { gateway = approve_up_to 100 } in
  let store, _sink = Store.open_protocol (Store.protocol shop.store) ~state:stale ~services in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer bugs);
  Store.add_invariant ~pos:__POS__ store "paid charges are kept" kept;
  match Observe.Bug_log.reports bugs with
  | [ { Observe.Bug_report.message; _ } ] -> assert (message = "2 paid, then 1 when added")
  | _ -> assert false
```

<!-- snippet: invariant-restart -->
```ocaml
let () =
  let shop = open_ () in
  Store.add_invariant ~pos:__POS__ shop.store "paid charges are kept" (never_forgets ());
  buy shop 4;
  let stale = Store.state shop.store in
  buy shop 6;
  (* The Store is replaced, and its successor starts from a snapshot: here a
     stale one, taken before the second charge was paid. The invariants move
     across with what they remember. *)
  let invariants = Store.take_invariants shop.store in
  Store.close shop.store;
  let services = { gateway = approve_up_to 100 } in
  let store, _sink = Store.open_protocol (Store.protocol shop.store) ~state:stale ~services in
  let bugs = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer bugs);
  Store.restore_invariants store invariants;
  match Observe.Bug_log.reports bugs with
  | [ { Observe.Bug_report.message; _ } ] -> assert (message = "2 paid, then 1 on restore")
  | _ -> assert false
```

<!-- snippet: report-dedupe -->
```ocaml
open Causet_reporting

let contains line part =
  let n = String.length part in
  let rec at i = i + n <= String.length line && (String.sub line i n = part || at (i + 1)) in
  at 0

(* A declined charge, as an error report deduplicated by its reason. *)
let declined amount reason =
  let payload, dedupe_key =
    Payload.create "charge_declined"
    |> Payload.field "amount" (Payload.uint amount)
    |> Payload.field "reason" (Payload.string reason)
    |> Payload.finish_with_dedupe_key ("charge_declined:" ^ reason)
  in
  Report.make ~dedupe_key Report.Error ~source:"payments" payload

let () =
  let pipeline = Pipeline.create () in
  (* Times are milliseconds of the Store's clock, so a test chooses them. *)
  let first = Pipeline.accept_report pipeline 1_000 (declined 30 "insufficient_funds") in
  let again = Pipeline.accept_report pipeline 1_200 (declined 45 "insufficient_funds") in
  let other = Pipeline.accept_report pipeline 1_300 (declined 10 "card_expired") in
  assert (Option.is_some first.record && Option.is_some other.record);
  assert (Option.is_none again.record) (* the same error, within the dedupe window *);
  let stats = Pipeline.stats pipeline in
  assert ((stats.accepted, stats.dropped_deduped) = (2, 1));
  (* One JSON line, ready to dump or send. *)
  let line = Report.Record.to_json (Option.get first.record) in
  assert (contains line {|"kind":"error","source":"payments"|});
  assert (contains line {|"event":"charge_declined","amount":30,"reason":"insufficient_funds"|})
```

<!-- snippet: report-aggregate -->
```ocaml
let () =
  let policy =
    {
      Config.Policy.source = "shop";
      event = "checkout";
      (* Aggregated only: no checkout report goes out on its own. *)
      sample_rate = Some 0.0;
      aggregate =
        Some
          {
            Config.Aggregate.window_ms = 60_000;
            fields = [ "took_ms" ];
            group_by = [ "ok" ];
            max_groups = 4;
            samples = 16;
          };
    }
  in
  let pipeline = Pipeline.create ~config:{ Config.default with metrics = [ policy ] } () in
  List.iteri
    (fun at (took_ms, ok) ->
      let payload =
        Payload.create "checkout"
        |> Payload.field "took_ms" (Payload.uint took_ms)
        |> Payload.field "ok" (Payload.bool ok)
        |> Payload.finish
      in
      let outcome =
        Pipeline.accept_report pipeline (1_000 + at) (Report.make Report.Metric ~source:"shop" payload)
      in
      assert (Option.is_none outcome.record))
    [ (120, true); (80, true); (95, true); (300, false) ];
  (* When the minute is up, one report sums it up. *)
  match Pipeline.flush_aggregates pipeline 61_000 with
  | [ aggregate ] ->
      let record = Option.get (Pipeline.accept_aggregate_report pipeline 61_000 aggregate) in
      let line = Report.Record.to_json record in
      assert (contains line {|"event":"metric_aggregate"|});
      assert (contains line {|"count":4,"min":80,"max":300,"avg":148.75|});
      assert (contains line {|"groups":{"ok":{"false":1,"true":3}}|})
  | _ -> assert false
```

<!-- snippet: report-service -->
<!-- $MDX file=examples/observability_orders.ml -->
```ocaml
(* The reporting Service: the pipeline, and where its records go. *)
type reporting = {
  pipeline : Causet_reporting.Pipeline.t;
  mutable delivered : Causet_reporting.Report.Record.t list;
}

type orders_action = Fail of { order : int } [@@deriving message ~name:"orders.action"]

(* What went wrong, as data: the reducer describes it, the Service builds the
   report. *)
type orders_report = Failed of { order : int }
[@@deriving message ~name:"orders.report"]

let orders : (int, reporting, orders_action, orders_report, never) Component.t =
  Component.make ~name:"orders" ~actions:orders_action_message
    ~effects:orders_report_message ~outputs:Message.never
    ~reduce:(fun ctx (Fail { order }) _ ->
      Ctx.update ctx (( + ) 1);
      Ctx.emit_effect ctx (Failed { order }))
    ~handle:(fun ctx (Failed { order }) (meta : Meta.t) ->
      let open Causet_reporting in
      let payload =
        Payload.create "order_failed"
        |> Payload.field "order" (Payload.uint order)
        |> Payload.finish
      in
      let report = Report.make Report.Error ~source:"orders" payload in
      (* Stamped with the execution's time, in milliseconds, never the system clock. *)
      let reporting = Effect_ctx.service ctx in
      let outcome =
        Pipeline.accept_report reporting.pipeline (meta.time / 1_000_000) report
      in
      reporting.delivered <- reporting.delivered @ Option.to_list outcome.record)
    ()
```

<!-- snippet: report-service-run -->
```ocaml
let () =
  let reporting = { pipeline = Pipeline.create (); delivered = [] } in
  let clock = Clock.Manual.clock (Clock.Manual.create 5_000_000_000) in
  let store, _sink = Store.start_exn ~clock orders ~state:0 ~services:reporting in
  let m = Store.mount_exn store (Path.root orders) in
  ignore (Store.dispatch_exn store m (Fail { order = 7 }));
  match (Store.services store).delivered with
  | [ record ] -> assert (record.timestamp_ms = 5_000)
  | _ -> assert false
```
