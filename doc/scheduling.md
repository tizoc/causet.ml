The causet guide's OCaml examples for its scheduling page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: opens -->
```ocaml
open Causet
open Causet_guide.Shop
```

<!-- snippet: steps-component -->
<!-- $MDX file=examples/scheduling_steps.ml -->
```ocaml
type step = Start | A | A_child | C | From_effect
[@@deriving message ~name:"steps.action"]

type b = B [@@deriving message ~name:"steps.effect"]

let steps : (unit, unit, step, b, never) Component.t =
  Component.make ~name:"steps" ~actions:step_message ~effects:b_message
    ~outputs:Message.never
    ~reduce:(fun ctx step _ ->
      match step with
      | Start ->
          Ctx.emit_action ctx A;
          Ctx.emit_effect ctx B;
          Ctx.emit_action ctx C
      | A -> Ctx.emit_action ctx A_child
      | A_child | C | From_effect -> ())
    ~handle:(fun ctx B _ -> ignore (Effect_ctx.dispatch ctx From_effect))
    ()
```

<!-- snippet: fifo-order -->
```ocaml
let () =
  let store, _sink = Store.start_exn steps ~state:() ~services:() in
  let mount = Store.mount_exn store (Path.root steps) in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  ignore (Store.dispatch_exn store mount Start);
  let trace = Array.of_list (Observe.Trace_log.entries log) in
  assert (
    Array.to_list (Array.map (fun (e : Observe.Trace_entry.t) -> e.kind) trace)
    = [ "steps.start"; "steps.a"; "steps.a_child"; "steps.b"; "steps.from_effect"; "steps.c" ]);
  (* Every execution has its own ID, strictly increasing... *)
  Array.iteri (fun i (e : Observe.Trace_entry.t) -> if i > 0 then assert (trace.(i - 1).id < e.id)) trace;
  (* ...and names the execution that caused it. *)
  assert (trace.(2).causal = trace.(1).id) (* A_child was emitted by A *);
  assert (trace.(4).causal = trace.(3).id) (* From_effect was dispatched by B *)
```

<!-- snippet: refused-at-root -->
```ocaml
let () =
  let shop = open_ () in
  (* An empty cart cannot check out. *)
  assert (Store.dispatch_to shop.store shop.cart_mount Checkout = Ok Disabled);
  assert (Store.executions shop.store = 0)
```

<!-- snippet: root-guard -->
```ocaml
let () =
  let reg = Registry.create () in
  Registry.root reg node;
  let sync_path = Path.child (Path.child (Path.root node) chain_slot) sync_slot in
  (* Sync, two levels down, may ask only a peer the node is connected to. *)
  Registry.admit reg sync_path (fun node _sync (Request { peer }) _now ->
      List.mem peer node.peers);
  let protocol = match Registry.freeze reg with Ok p -> p | Error _ -> assert false in
  let state = { empty_node with peers = [ 1; 2 ] } in
  let store, _sink = Store.open_protocol protocol ~state ~services:() in
  let sync = Store.mount_exn store sync_path in
  assert (Store.dispatch_to store sync (Request { peer = 1 }) = Ok Executed);
  assert (Store.dispatch_to store sync (Request { peer = 9 }) = Ok Disabled);
  assert ((Store.state store).chain.sync.asked = [ 1 ])
```

<!-- snippet: drain-events -->
```ocaml
let () =
  let shop = open_ () in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 500 }));
  (* The gateway has answered; the answer waits on the bus until drained. *)
  assert (Pending.cardinal (Store.state shop.store).payments.pending = 1);
  assert (Store.drain_events shop.store 16 = Ok 1);
  assert ((Store.state shop.store).payments.declined = [ 500 ])
```

<!-- snippet: fragile-component -->
<!-- $MDX file=examples/scheduling_fragile.ml -->
```ocaml
type fragile = Fine | Break [@@deriving message ~name:"fragile.action"]

let glass : (int, unit, fragile, never, never) Component.t =
  Component.make ~name:"glass" ~actions:fragile_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (( + ) 1);
      match action with Break -> failwith "broke" | Fine -> ())
    ()
```

<!-- snippet: fault -->
```ocaml
let () =
  let store, _sink = Store.start_exn glass ~state:0 ~services:() in
  let mount = Store.mount_exn store (Path.root glass) in
  assert (Store.dispatch_to store mount Break = Error (Faulted "broke"));
  assert (Store.dispatch_to store mount Fine = Error (Faulted "broke"));
  assert (Store.state store = 1) (* the reducer's change before the raise stays *);
  assert (Store.fault store = Some "broke")
```

<!-- snippet: shelf-component -->
<!-- $MDX file=examples/scheduling_shelf.ml -->
```ocaml
module String_map = Map.Make (String)

type shelf = {
  titles : string array;
  index : int String_map.t;  (** Positions in [titles], by title. *)
}

(* A helper with no context: the report finds its Store by itself. *)
let take shelf title =
  match String_map.find_opt title shelf.index with
  | None -> (shelf, None)
  | Some at ->
      let shelf = { shelf with index = String_map.remove title shelf.index } in
      if at >= Array.length shelf.titles then (
        Bug.condition __POS__
          ~fields:[ ("title", title); ("at", string_of_int at) ]
          "the index names a title the shelf does not have";
        (shelf, None) (* the fallback: as if it were not there *))
      else (shelf, Some shelf.titles.(at))

type shelf_action = Take of string [@@deriving message ~name:"shelf.action"]

let shelves : (shelf, unit, shelf_action, never, never) Component.t =
  Component.make ~name:"shelves" ~actions:shelf_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Take title) _ -> Ctx.update ctx (fun s -> fst (take s title)))
    ()
```

<!-- snippet: bug-report -->
```ocaml
let () =
  let broken = { titles = [||]; index = String_map.singleton "Dune" 3 } in
  let store, _sink = Store.start_exn shelves ~state:broken ~services:() in
  let mount = Store.mount_exn store (Path.root shelves) in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  (* The Store keeps running: the fallback ran. *)
  assert (Store.dispatch_to store mount (Take "Dune") = Ok Executed);
  match Observe.Bug_log.reports log with
  | [ { place = Some { phase = Reducer; path = "/"; _ }; fields; _ } ] ->
      assert (List.assoc "at" fields = "3")
  | _ -> assert false
```
