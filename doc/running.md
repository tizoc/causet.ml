The causet guide's OCaml examples for its running page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: start -->
```ocaml
open Causet
open Causet_guide.Shop

let () =
  let services = { gateway = approve_up_to 100 } in
  let store, _sink = Store.start_exn shop_root ~state:empty ~services in
  let cart = Result.get_ok (Store.mount store (Path.child (Path.root shop_root) cart_slot)) in
  ignore (Store.dispatch_exn store cart (Add { item = "tea"; price = 4 }));
  assert ((Store.state store).cart.total = 4)
```

<!-- snippet: assemble -->
```ocaml
open Causet
open Causet_guide.Shop

(* The shop, assembled by hand: a Protocol, its bus, a Store and its mounts. *)
let open_with ?(worker_capacity = 64) gateway =
  let registry = Registry.create () in
  (* Registers the whole tree. *)
  Registry.root registry shop_root;
  let proto =
    match Registry.freeze registry with
    | Ok proto -> proto
    | Error problems -> failwith (String.concat "; " (List.map problem_message problems))
  in
  let sink, inbox = Event_bus.create proto ~worker_capacity in
  let store =
    match Store.create proto ~state:empty ~services:{ gateway } inbox with
    | Ok store -> store
    | Error _ -> failwith "the bus belongs to another Protocol"
  in
  let at slot = Protocol.resolve_exn proto (Path.child (Path.root shop_root) slot) in
  { store; sink; cart_mount = at cart_slot; payments_mount = at payments_slot }

let () =
  let shop = open_with (approve_up_to 100) in
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  ignore (Store.drain_events shop.store 16);
  assert ((Store.state shop.store).payments.paid = [ 4 ])
```

<!-- snippet: event-readiness -->
```ocaml
let () =
  let shop = open_with (approve_up_to 100) in
  let ready = Store.readiness shop.store in
  (* An event-loop integration would signal its driver here. *)
  let notified = Atomic.make false in
  let subscription =
    match Readiness.subscribe ready (fun () -> Atomic.set notified true) with
    | Ok sub -> sub | Error Already_subscribed -> assert false
  in
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  assert (Atomic.get notified);
  assert (Readiness.current ready = Some Readiness.Events);
  ignore (Store.drain_events shop.store 64);
  Readiness.cancel subscription;
  Store.close shop.store

```

<!-- snippet: blocking-readiness -->
```ocaml
let () =
  let shop = open_with (approve_up_to 100) in
  let waiter =
    match Readiness.Blocking.create (Store.readiness shop.store) with
    | Ok waiter -> waiter | Error Already_subscribed -> assert false
  in
  Fun.protect ~finally:(fun () -> Readiness.Blocking.close waiter; Store.close shop.store)
    (fun () ->
      assert (Readiness.Blocking.wait_timeout waiter ~timeout:0. = None);
      ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
      ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
      assert (Readiness.Blocking.wait_timeout waiter ~timeout:1. = Some Readiness.Events);
      assert ((Store.state shop.store).payments.paid = []);
      ignore (Store.drain_events shop.store 64);
      assert ((Store.state shop.store).payments.paid = [ 4 ]))
```

<!-- snippet: registration-problems -->
```ocaml
let () =
  let first = Child.make "cart" cart in
  let second = Child.make "cart" cart in
  let pay = Child.make "payments" payments in
  let cart_lens = Lens.field ~get:(fun s -> s.cart) ~set:(fun s cart -> { s with cart }) in
  (* A root that gets its wiring wrong three ways. *)
  let miswired =
    Component.make ~name:"miswired" ~actions:shop_action_message ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun _ _ _ -> ())
      ~register:(fun r ->
        (* The cart reports Outputs, and nothing lowers them. *)
        Registrar.child r first ~lens:cart_lens ();
        (* A second slot with the same name. *)
        Registrar.child r second ~lens:cart_lens ();
        (* Payments has Effects, and no Service projection reaches it. *)
        Registrar.child r pay
          ~lens:
            (Lens.field
               ~get:(fun s -> s.payments)
               ~set:(fun s payments -> { s with payments }))
          ())
      ()
  in
  let registry : (shop_state, shop_services) Registry.t = Registry.create () in
  Registry.root registry miswired;
  assert (
    Registry.freeze registry
    = Error
        [
          No_lowerer "/cart";
          Slot_bound_twice "/cart";
          No_service_projection { path = "/payments"; at = "/payments" };
        ])
```

<!-- snippet: store-errors -->
```ocaml
let () =
  (* A bus made for one Protocol, given to a Store of another. *)
  let _sink, inbox = Event_bus.create (protocol ()) ~worker_capacity:64 in
  let services = { gateway = approve_up_to 100 } in
  match Store.create (protocol ()) ~state:empty ~services inbox with
  | Error Foreign_bus -> ()
  | _ -> assert false
```

<!-- snippet: driver-loop -->
```ocaml
(* Answers each charge from a thread of its own. *)
let on_threads =
  {
    charge =
      (fun request amount reply ->
        (* The reply moves to the worker thread, which answers when it is done. *)
        ignore
          (Thread.create
             (fun () ->
               let answer = { request; approved = amount <= 100 } in
               (* Waits while the worker lane is full; fails once the Store is gone. *)
               ignore (Reply.send_timeout reply answer ~timeout:5.0))
             ()));
  }

let () =
  (* A worker lane of 2, for 5 charges: some answers wait for room. *)
  let shop = open_with ~worker_capacity:2 on_threads in
  List.iter
    (fun amount -> ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount })))
    [ 10; 20; 30; 40; 500 ];
  (* A dedicated native driver waits on the bus between bounded drains. *)
  let waiter =
    match Readiness.Blocking.create (Store.readiness shop.store) with
    | Ok waiter -> waiter | Error Already_subscribed -> assert false
  in
  let deadline = Unix.gettimeofday () +. 10.0 in
  Fun.protect ~finally:(fun () -> Readiness.Blocking.close waiter)
    (fun () ->
      while not (Pending.is_empty (Store.state shop.store).payments.pending) do
        assert (Unix.gettimeofday () < deadline);
        match Store.drain_events shop.store 64 with
        | Ok 0 -> ignore (Readiness.Blocking.wait_timeout waiter
            ~timeout:(max 0. (deadline -. Unix.gettimeofday ())))
        | Ok _ -> ()
        | Error _ -> assert false
      done);
  (* The threads answered in any order. *)
  let { paid; declined; _ } = (Store.state shop.store).payments in
  assert (List.sort compare paid = [ 10; 20; 30; 40 ]);
  assert (declined = [ 500 ])
```

<!-- snippet: worker-lane-full -->
```ocaml
(* Keeps each charge's reply, for a worker to answer later. *)
let parked () =
  let replies = ref [] in
  ({ charge = (fun request _ reply -> replies := !replies @ [ (request, reply) ]) }, replies)

let () =
  let gateway, replies = parked () in
  (* Room for one Event. *)
  let shop = open_with ~worker_capacity:1 gateway in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 10 }));
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 20 }));
  let[@warning "-8"] [ (first, a); (second, b) ] = !replies in
  (* A worker answers both, while the Store drains nothing. *)
  let worker =
    Thread.create
      (fun () ->
        assert (Reply.try_send a { request = first; approved = true } = Ok ());
        (* The lane is full: the Event is handed back, to retry, drop or report. *)
        match Reply.try_send b { request = second; approved = true } with
        | Error (Full answer) ->
            (* Waiting for room gives up after its timeout. *)
            assert (Reply.send_timeout b answer ~timeout:0.01 = Error (Full answer))
        | _ -> assert false)
      ()
  in
  Thread.join worker;
  assert (Store.drain_events shop.store 16 = Ok 1);
  assert ((Store.state shop.store).payments.paid = [ 10 ]);
  (* Its answer was never sent. *)
  assert (Pending.cardinal (Store.state shop.store).payments.pending = 1)
```

<!-- snippet: send-front -->
```ocaml
(* Answers at once, ahead of everything waiting in the Store lane. *)
let urgent =
  {
    charge =
      (fun request _ reply ->
        match Reply.send_front reply { request; approved = true } with
        | Ok () -> ()
        | Error _ -> failwith "the Store is closed");
  }

let () =
  let shop = open_with urgent in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 1 }));
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 2 }));
  ignore (Store.drain_events shop.store 16);
  (* The second answer went ahead of the first. *)
  assert ((Store.state shop.store).payments.paid = [ 2; 1 ])
```

<!-- snippet: shutdown -->
```ocaml
let () =
  let gateway, replies = parked () in
  let shop = open_with gateway in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 10 }));
  let request, reply = List.hd !replies in
  Store.close shop.store;
  (* Every later send fails, on every thread, and hands the Event back. *)
  let answer = { request; approved = true } in
  assert (Reply.try_send reply answer = Error (Closed answer));
  assert (Sink.try_send_to shop.sink shop.payments_mount charged_message answer
          = Error (Closed answer))
```
