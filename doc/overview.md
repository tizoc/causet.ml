The causet guide's OCaml examples for its overview page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: checkout -->
```ocaml
open Causet
open Causet_guide.Shop

let () =
  let shop = open_ () in
  ignore (Store.dispatch_exn shop.store shop.cart_mount (Add { item = "tea"; price = 4 }));
  ignore (Store.dispatch_exn shop.store shop.cart_mount Checkout);
  (* Sent, not answered yet. *)
  assert (Pending.cardinal (Store.state shop.store).payments.pending = 1);
  (* The gateway's answer runs. *)
  ignore (Store.drain_events shop.store 16);
  assert ((Store.state shop.store).payments.paid = [ 4 ])
```

<!-- snippet: reducer -->
<!-- $MDX skip -->
```ocaml
~reduce:(fun ctx action _ ->
  match action with
  | Charge { amount } ->
      let request = Ctx.new_request ctx in
      Ctx.update ctx (fun s -> { s with pending = Pending.add request amount s.pending });
      Ctx.emit_effect ctx (Charge_effect { request; amount })
  | Settled { request; approved } -> (
      let s = Ctx.state ctx in
      (* A duplicate, or an answer after a timeout, finds nothing: it is stale. *)
      match Pending.take request s.pending with
      | None -> ()
      | Some (amount, pending) ->
          Ctx.set_state ctx
            (if approved then { s with pending; paid = s.paid @ [ amount ] }
             else { s with pending; declined = s.declined @ [ amount ] })))
```

<!-- snippet: deliver-out-of-order -->
```ocaml
module Node = Causet_test.Node

let () =
  let node =
    Node.create ~name:"shop" payments
      ~state:{ pending = Pending.empty; paid = []; declined = [] }
      ~services:(approve_up_to 100) ~session:1 (Clock.Manual.create 0)
  in
  let m = Node.mount node (Path.root payments) in
  ignore (Node.dispatch node m (Charge { amount = 30 }));
  ignore (Node.dispatch node m (Charge { amount = 200 }));
  (* The second answer arrives first; each still settles its own charge. *)
  assert (Node.deliver_where node charged_message (fun c -> not c.approved));
  assert ((Node.state node).declined = [ 200 ]);
  ignore (Node.deliver_next node);
  assert ((Node.state node).paid = [ 30 ])
```

## Find bugs automatically

<!-- snippet: find-bug -->
```ocaml
open Causet
open Causet_guide.Shop
open Causet_test

let spending rng ~forgets =
  let remaining = ref 100 in
  let gateway =
    {
      charge =
        (fun request amount reply ->
          let approved = amount <= !remaining in
          (* The planted bug: earlier approvals are forgotten. *)
          if approved && not forgets then remaining := !remaining - amount;
          assert (Reply.try_send reply { request; approved } = Ok ()));
    }
  in
  let node =
    Node.create ~name:"shop" payments ~state:{ pending = Pending.empty; paid = []; declined = [] }
      ~services:gateway ~session:1 (Clock.Manual.create 0)
  in
  let node =
    Node.check node "paid within budget" (fun s ->
        if List.fold_left ( + ) 0 s.paid <= 100 then Ok () else Error "overspent")
  in
  let mount = Node.mount node (Path.root payments) in
  while Rng.more rng do
    if Rng.chance rng 1 2 then
      ignore (Node.dispatch node mount (Charge { amount = Rng.between rng 1 100 }))
    else ignore (Node.deliver_next node)
  done;
  while Option.is_some (Node.deliver_next node) do
    ()
  done

let () =
  check (fun rng -> spending rng ~forgets:false); (* The fixed gateway passes. *)
  let found = Option.get (search default (fun rng -> spending rng ~forgets:true)) in
  assert (found.key = {|node shop: invariant "paid within budget"|});
  assert (List.length found.choices < found.original);
  Printf.printf "seed %d: %s\nshrunk %d choices to %d\nchoices [%s]\n" found.seed found.key
    found.original (List.length found.choices)
    (String.concat ", " (List.map string_of_int found.choices));
  (* Replay must reproduce the planted failure. *)
  try
    replay found.choices (fun rng -> spending rng ~forgets:true);
    assert false
  with Failed failure -> assert (failure.key = found.key)
```

```text
seed 1: node shop: invariant "paid within budget"
shrunk 13 choices to 7
choices [1, 1, 51, 1, 1, 48, 0]
```
