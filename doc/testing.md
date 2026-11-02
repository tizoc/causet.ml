The causet guide's OCaml examples for its testing page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: opens -->
```ocaml
open Causet
open Causet_guide.Shop
open Causet_test
```

<!-- snippet: one-node -->
```ocaml
let every_order_is_accounted_for (s : shop_state) =
  let p = s.payments in
  let ended = List.length p.paid + List.length p.declined + Pending.cardinal p.pending in
  if ended = s.orders then Ok () else Error (Printf.sprintf "%d orders, %d charges" s.orders ended)

let cart_path = Path.child (Path.root shop_root) cart_slot

let () =
  let shop =
    Node.of_protocol ~name:"shop" (protocol ()) ~state:empty
      ~services:{ gateway = approve_up_to 100 } ~session:1 (Clock.Manual.create 1_000)
  in
  let shop = Node.check shop "every order is paid, declined or pending" every_order_is_accounted_for in
  let cart = Node.mount shop cart_path in
  ignore (Node.dispatch shop cart (Add { item = "tea"; price = 4 }));
  ignore (Node.dispatch shop cart Checkout);
  (* The gateway answered at once, but its answer is held. *)
  assert (List.length (Node.held shop) = 1);
  assert (Pending.cardinal (Node.state shop).payments.pending = 1);
  ignore (Node.deliver_next shop);
  assert ((Node.state shop).payments.paid = [ 4 ])
```

<!-- snippet: shared-world -->
```ocaml
let ms = 1_000_000
let budget = 200

(* The bank every shop's gateway reaches. *)
type bank = {
  world : Rng.t;  (** The world's own stream: the answers' delays. *)
  clock : Clock.Manual.m;
  flight : (charged reply * charged, unit) In_flight.t;
  mutable approved : int;
  forgets : bool;  (** The planted bug: approvals are not counted. *)
}

(* The gateway of every shop. *)
let gateway_of bank =
  {
    charge =
      (fun request amount reply ->
        let approved = bank.approved + amount <= budget in
        if approved && not bank.forgets then bank.approved <- bank.approved + amount;
        let at = Clock.Manual.get bank.clock + (Rng.between bank.world 1 50 * ms) in
        In_flight.send_at bank.flight at (reply, { request; approved }));
  }

(* The test's view of the same bank. *)
let world_of bank =
  {
    next_time = (fun () -> In_flight.next_time bank.flight);
    advance =
      (fun now ->
        List.iter
          (fun (reply, answer) ->
            match Reply.try_send_as_worker reply answer with
            | Ok () -> ()
            | Error _ -> failwith "the held bus takes it")
          (In_flight.due bank.flight now));
  }

type step = Deliver | Add_tea | Check_out

let shops rng ~forgets =
  let scenario = Scenario.with_rng rng (1_000 * ms) in
  let bank =
    { world = Rng.split rng; clock = (Scenario.clock scenario); flight = In_flight.create (); approved = 0; forgets }
  in
  let world = world_of bank in
  let group = Group.create "shops" in
  Group.check group "paid within the budget" (fun views _ ->
      let paid = List.fold_left (fun sum (_, paid) -> sum + paid) 0 views in
      if paid <= budget then Ok () else Error (Printf.sprintf "%d paid" paid));
  let protocol = protocol () in
  let nodes =
    Array.init (Rng.between rng 2 3) (fun i ->
        let node =
          Node.of_protocol ~name:(Printf.sprintf "shop %d" i) protocol ~state:empty
            ~services:{ gateway = gateway_of bank } ~session:1 (Scenario.clock scenario)
        in
        Node.publish node group (fun s -> List.fold_left ( + ) 0 s.payments.paid))
  in
  let driven_nodes = Array.to_list (Array.map driven nodes) in
  let cart = Node.mount nodes.(0) cart_path in
  let swarm = Swarm.create rng [ (Deliver, 3); (Add_tea, 2); (Check_out, 1) ] in
  while Rng.more rng do
    let shop = nodes.(Rng.below rng (Array.length nodes)) in
    match Swarm.pick swarm rng with
    | Deliver -> ignore (Scenario.step scenario driven_nodes world)
    | Add_tea -> ignore (Node.dispatch shop cart (Add { item = "tea"; price = Rng.between rng 1 90 }))
    | Check_out -> ignore (Node.dispatch shop cart Checkout)
  done;
  let stop = Scenario.run_until scenario driven_nodes world ~steps:1_000 ~until:(fun () -> false) in
  assert (stop = Quiet);
  Array.iter
    (fun node ->
      if not (Pending.is_empty (Node.state node).payments.pending) then
        failwith (Node.name node ^ ": a charge never answered"))
    nodes

let () =
  check (fun rng -> shops rng ~forgets:false);
  match search default (fun rng -> shops rng ~forgets:true) with
  | Some found ->
      assert (found.key = {|group shops: invariant "paid within the budget"|});
      assert (List.length found.choices < found.original)
  | None -> assert false
```

<!-- snippet: check-with -->
```ocaml
(* A shop takes one payment, then restarts from what it keeps. *)
let pay_then_restart ~keeps =
  let services () = { gateway = approve_up_to 100 } in
  (* The invariant's memory: what was paid at its last step. *)
  let before = ref 0 in
  let shop =
    Node.of_protocol ~name:"shop" (protocol ()) ~state:empty ~services:(services ()) ~session:1
      (Clock.Manual.create 1_000)
  in
  let shop =
    Node.check_with shop "paid only by answers, never forgotten" (fun s step ->
        let paid = List.fold_left ( + ) 0 s.payments.paid in
        let was = !before in
        before := paid;
        let ok =
          match (step : invariant_step) with
          (* A fixture may start with payments made. *)
          | Added -> true
          | Settled_step { kind = "payments.charged"; _ } -> paid >= was
          | Settled_step _ | Restored -> paid = was
        in
        if ok then Ok () else Error (Printf.sprintf "paid %d, then %d" was paid))
  in
  let cart = Node.mount shop cart_path in
  ignore (Node.dispatch shop cart (Add { item = "tea"; price = 4 }));
  ignore (Node.dispatch shop cart Checkout);
  ignore (Node.deliver_next shop);
  let kept = if keeps then Node.state shop else empty in
  Node.restart shop ~state:kept ~services:(services ()) ~session:2

let () =
  pay_then_restart ~keeps:true;
  match search default (fun _ -> pay_then_restart ~keeps:false) with
  | Some found -> assert (found.key = {|node shop: invariant "paid only by answers, never forgotten"|})
  | None -> assert false
```

<!-- snippet: group-steps -->
```ocaml
(* Shop a is paid 4 and closes; shop b restarts, then is paid [price].
   Returns the steps the group's check ran at. *)
let two_shops price =
  let group = Group.create "shops" in
  let steps = ref [] in
  (* The check's memory: each shop's paid total, kept after the shop has gone. *)
  let last = Hashtbl.create 2 in
  Group.check group "paid within 10, gone shops included" (fun views step ->
      let seen =
        match step with
        | Added -> "added"
        | Published node -> node ^ " published"
        | Settled node -> node ^ " settled"
        | Restarted node -> node ^ " restarted"
      in
      steps := seen :: !steps;
      List.iter (fun (node, paid) -> Hashtbl.replace last node paid) views;
      let paid = Hashtbl.fold (fun _ paid sum -> sum + paid) last 0 in
      if paid <= 10 then Ok () else Error (Printf.sprintf "%d paid" paid));
  let protocol = protocol () and clock = Clock.Manual.create 1_000 in
  let services () = { gateway = approve_up_to 100 } in
  let open_ name =
    let shop =
      Node.of_protocol ~name protocol ~state:empty ~services:(services ()) ~session:1 clock
    in
    Node.publish shop group (fun s -> List.fold_left ( + ) 0 s.payments.paid)
  in
  let cart = Protocol.resolve_exn protocol cart_path in
  let pay shop price =
    ignore (Node.dispatch shop cart (Add { item = "tea"; price }));
    ignore (Node.dispatch shop cart Checkout);
    ignore (Node.deliver_next shop)
  in
  let a = open_ "a" and b = open_ "b" in
  pay a 4;
  (* A closed node's view leaves its groups, and no check runs. *)
  Node.close a;
  Node.restart b ~state:(Node.state b) ~services:(services ()) ~session:2;
  pay b price;
  List.rev !steps

let () =
  let steps = two_shops 5 in
  assert (List.filteri (fun i _ -> i < 4) steps = [ "added"; "a published"; "b published"; "a settled" ]);
  assert (List.filteri (fun i _ -> i >= 6) steps = [ "b restarted"; "b settled"; "b settled"; "b settled" ]);
  match search default (fun _ -> ignore (two_shops 7)) with
  | Some found -> assert (found.key = {|group shops: invariant "paid within 10, gone shops included"|})
  | None -> assert false
```

<!-- snippet: choose-order -->
```ocaml
let () =
  let scenario = Scenario.create 1 1_000 in
  let protocol = protocol () in
  let nodes =
    Array.init 2 (fun i ->
        Node.of_protocol ~name:(Printf.sprintf "shop %d" i) protocol ~state:empty
          ~services:{ gateway = approve_up_to 100 } ~session:1 (Scenario.clock scenario))
  in
  let driven_nodes = Array.to_list (Array.map driven nodes) in
  let cart = Node.mount nodes.(0) cart_path in
  List.iter
    (fun (shop, price) ->
      ignore (Node.dispatch nodes.(shop) cart (Add { item = "tea"; price }));
      ignore (Node.dispatch nodes.(shop) cart Checkout))
    [ (0, 4); (0, 5); (1, 6) ];
  Scenario.note scenario "test: three checkouts, every answer held";
  let pending = Scenario.pending scenario driven_nodes no_world in
  assert (String.starts_with ~prefix:"at 1000:\n  shop 0: 2 held, next timer none\n" pending);
  (* Any held Event of any node, chosen by the scenario's generator. *)
  let any held rng = Rng.below rng (List.length held) in
  while Scenario.step_with scenario driven_nodes no_world ~choose:any <> None do () done;
  (* Shop 1 first, then shop 0's second answer before its first. *)
  assert ((Node.state nodes.(0)).payments.paid = [ 5; 4 ]);
  assert (
    Scenario.report scenario 2
    = "scenario seed 1 failed after 4 steps; the last 2:\n\
      \     3. shop 0: deliver payments.charged to /payments\n\
      \     4. shop 0: deliver payments.charged to /payments");
  match Scenario.log scenario with
  | note :: first :: _ ->
      assert (note = "test: three checkouts, every answer held");
      assert (first = "shop 1: deliver payments.charged to /payments")
  | _ -> assert false
```

<!-- snippet: queue-order -->
```ocaml
(* Any node that holds something, chosen by the generator; within it, the
   Event its bus would run next. *)
let any_node held rng =
  let node = fst (List.nth held (Rng.below rng (List.length held))) in
  let first = Option.get (List.find_index (fun (n, _) -> n = node) held) in
  let own = List.filter_map (fun (n, e) -> if n = node then Some e else None) held in
  first + Option.get (next_in_queue_order own)

let () =
  for seed = 0 to 19 do
    let scenario = Scenario.create seed 1_000 in
    let protocol = protocol () in
    let nodes =
      Array.init 2 (fun i ->
          Node.of_protocol ~name:(Printf.sprintf "shop %d" i) protocol ~state:empty
            ~services:{ gateway = approve_up_to 100 } ~session:1 (Scenario.clock scenario))
    in
    let cart = Node.mount nodes.(0) cart_path in
    List.iter
      (fun (shop, price) ->
        ignore (Node.dispatch nodes.(shop) cart (Add { item = "tea"; price }));
        ignore (Node.dispatch nodes.(shop) cart Checkout))
      [ (0, 4); (0, 5); (1, 6) ];
    let driven_nodes = Array.to_list (Array.map driven nodes) in
    while Scenario.step_with scenario driven_nodes no_world ~choose:any_node <> None do () done;
    (* Whichever shop goes first, each settles its own answers in order. *)
    assert ((Node.state nodes.(0)).payments.paid = [ 4; 5 ])
  done
```

<!-- snippet: mid-workflow -->
```ocaml
(* A gateway that takes a charge and answers only when the test says. *)
let silent = { charge = (fun _ _ _ -> ()) }

let () =
  (* As if a checkout of 30 had run: its Effect, execution 7, made the
     request, and the charge is with the gateway. *)
  let request = { Request_id.session = 1; execution = 7; index = 0 } in
  let pending = Pending.add request 30 Pending.empty in
  let state = { empty with orders = 1; payments = { empty.payments with pending } } in
  let shop =
    Node.of_protocol ~name:"shop" (protocol ()) ~state ~services:{ gateway = silent } ~session:1
      (Clock.Manual.create 1_000)
  in
  let at_payments = Node.mount shop (Path.child (Path.root shop_root) payments_slot) in
  let refunds = Path.child (Path.root shop_root) (Child.make "refunds" payments) in
  assert (Result.is_error (Protocol.resolve (Node.protocol shop) refunds));
  (* The reply that Effect would have handed the gateway. The gateway answers
     twice, from what stands in for its worker thread. *)
  let reply = Option.get (Sink.reply_for (Node.sink shop) at_payments charged_message ~answers:7) in
  for _ = 1 to 2 do
    assert (Reply.try_send_as_worker reply ({ request; approved = true } : charged) = Ok ())
  done;
  match Node.held shop with
  | [ first; second ] ->
      assert ((first.held_answers, first.held_lane, first.held_path) = (Some 7, Worker_lane, "/payments"));
      let approved = Option.map (fun (c : charged) -> c.approved) (Node.inspect shop second.held_id charged_message) in
      assert (approved = Some true);
      (* The second answer is lost on the way; the first settles the charge. *)
      assert (Node.discard shop second.held_id);
      Node.deliver shop first.held_id;
      assert ((Node.state shop).payments.paid = [ 30 ]);
      assert (Node.held shop = [] && Pending.is_empty (Node.state shop).payments.pending)
  | _ -> assert false
```

<!-- snippet: held-bus -->
```ocaml
let () =
  let proto = protocol () in
  let _sink, inbox, held = Event_bus.held proto ~worker_capacity:64 1 in
  let store = Store.create_exn proto ~state:empty ~services:{ gateway = approve_up_to 100 } inbox in
  let cart = Protocol.resolve_exn proto cart_path in
  List.iter
    (fun price ->
      ignore (Store.dispatch_exn store cart (Add { item = "tea"; price }));
      ignore (Store.dispatch_exn store cart Checkout))
    [ 4; 200 ];
  (* Both answers are held: a drain finds nothing to run. *)
  assert (Store.drain_events store 16 = Ok 0);
  match Held.list held with
  | [ approved; declined ] ->
      let answer = Held.inspect held declined.held_id charged_message in
      assert (Option.map (fun (c : charged) -> c.approved) answer = Some false);
      assert (Held.discard held declined.held_id);
      assert (Held.release held approved.held_id = Ok ());
      assert (Store.drain_events store 16 = Ok 1);
      assert ((Store.state store).payments.paid = [ 4 ]);
      (* The declined charge's answer was lost: it is still pending. *)
      assert (Pending.cardinal (Store.state store).payments.pending = 1)
  | _ -> assert false
```
