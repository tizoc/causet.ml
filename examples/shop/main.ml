(* Start here: one checkout, then the same Payments component in a simulation. *)
open Causet
open Causet_guide.Shop
module Node = Causet_test.Node

let amounts values = "[" ^ String.concat ", " (List.map string_of_int values) ^ "]"

let () =
  let shop = open_ () in
  let signalled = Atomic.make false in
  (* A host integration would signal its driver here. *)
  let subscription =
    match
      Readiness.subscribe (Store.readiness shop.store) (fun () ->
          Atomic.set signalled true)
    with
    | Ok sub -> sub
    | Error Already_subscribed -> assert false
  in
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  let payment_state = (Store.state shop.store).payments in
  assert (Pending.cardinal payment_state.pending = 1);
  assert (Atomic.get signalled);
  assert (payment_state.paid = []);
  Printf.printf "before delivery: pending=%d, paid=%s\n"
    (Pending.cardinal payment_state.pending)
    (amounts payment_state.paid);
  assert (Store.drain_events shop.store 16 = Ok 1);
  let payment_state = (Store.state shop.store).payments in
  assert (Pending.is_empty payment_state.pending);
  assert (payment_state.paid = [ 4 ]);
  Printf.printf "after delivery: pending=%d, paid=%s\n"
    (Pending.cardinal payment_state.pending)
    (amounts payment_state.paid);
  Readiness.cancel subscription;
  Store.close shop.store;

  (* The same component, with replies held until the test chooses their order. *)
  let node =
    Node.create ~name:"payments" payments
      ~state:{ pending = Pending.empty; paid = []; declined = [] }
      ~services:(approve_up_to 100) ~session:1 (Clock.Manual.create 0)
  in
  let mount = Node.mount node (Path.root payments) in
  ignore (Node.dispatch node mount (Charge { amount = 30 }));
  ignore (Node.dispatch node mount (Charge { amount = 200 }));
  assert (Node.deliver_where node charged_message (fun answer -> not answer.approved));
  assert ((Node.state node).declined = [ 200 ]);
  ignore (Node.deliver_next node);
  assert ((Node.state node).paid = [ 30 ]);
  assert (Pending.is_empty (Node.state node).pending);
  Printf.printf "simulation: declined=%s, paid=%s\n"
    (amounts (Node.state node).declined)
    (amounts (Node.state node).paid)
