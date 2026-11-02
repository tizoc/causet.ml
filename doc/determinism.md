The causet guide's OCaml examples for its determinism page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: repeat-run -->
```ocaml
open Causet
open Causet_guide.Shop

let run () =
  let shop = open_ ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000)) () in
  let log = Observe.Trace_log.create () in
  Store.add_observer shop.store (Observe.Trace_log.observer log);
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  let trace =
    List.map
      (fun (e : Observe.Trace_entry.t) -> (e.id, e.causal, e.kind, e.path))
      (Observe.Trace_log.entries log)
  in
  (trace, Store.state shop.store)

(* Two runs: the same executions in the same order, the same end. *)
let () = assert (run () = run ())
```

<!-- snippet: pending-session -->
```ocaml
let pending () =
  let shop = open_ ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000)) () in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 7 }));
  fst (List.hd (Pending.bindings (Store.state shop.store).payments.pending))
  (* the answer is not drained *)

let () =
  let first = pending () in
  let second = pending () in
  assert (first.session <> second.session) (* each run's own *);
  assert ((first.execution, first.index) = (second.execution, second.index))
```

<!-- snippet: stamps -->
<!-- $MDX file=examples/determinism_stamps.ml -->
```ocaml
type stamp = Now [@@deriving message ~name:"stamp.action"]

(* Keeps the time of every Action it reduced. *)
let stamps : (int list, unit, stamp, never, never) Component.t =
  Component.make ~name:"stamps" ~actions:stamp_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Now (meta : Meta.t) ->
      (* The execution's time, never the system clock's. *)
      Ctx.update ctx (fun times -> times @ [ meta.time ]))
    ()
```

<!-- snippet: stamps-run -->
```ocaml
let () =
  let clock = Clock.Manual.create 1_000 in
  let store, _sink = Store.start_exn ~clock:(Clock.Manual.clock clock) stamps ~state:[] ~services:() in
  let m = Store.mount_exn store (Path.root stamps) in
  ignore (Store.dispatch_exn store m Now);
  ignore (Store.dispatch_exn store m Now) (* the clock has not moved *);
  Clock.Manual.set clock 5_000;
  ignore (Store.dispatch_exn store m Now);
  Clock.Manual.set clock 10 (* a clock that steps back *);
  ignore (Store.dispatch_exn store m Now);
  assert (Store.state store = [ 1_000; 1_000; 5_000; 5_000 ])
```
