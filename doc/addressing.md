The causet guide's OCaml examples for its addressing page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: resolve-mount -->
```ocaml
open Causet
open Causet_guide.Shop

let () =
  let protocol = protocol () in
  let cart : (cart_state, unit, cart_action, never, cart_output) mount =
    Protocol.resolve_exn protocol (Path.child (Path.root shop_root) cart_slot)
  in
  assert (Protocol.path_of protocol cart = "/cart")
(* A cart mount accepts only cart actions: the type comes from the slot. *)
```

<!-- snippet: foreign-mount -->
```ocaml
let () =
  let shop = open_ () in
  let elsewhere = protocol () (* frozen separately: another Protocol *) in
  let foreign = Protocol.resolve_exn elsewhere (Path.child (Path.root shop_root) cart_slot) in
  assert (Store.dispatch_to shop.store foreign Checkout = Error Foreign_mount)
```

<!-- snippet: pair -->
<!-- $MDX file=examples/addressing_pair.ml -->
```ocaml
type inc = Inc [@@deriving message ~name:"counter.action"]

(* A reusable leaf, mounted twice below. *)
let counter : (int, unit, inc, never, never) Component.t =
  Component.make ~name:"counter" ~actions:inc_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Inc _ -> Ctx.update ctx (( + ) 1))
    ()

let left = Child.make "left" counter
let right = Child.make "right" counter

type pair_state = {
  opened : bool;
  left : int;
  right : int option;  (** Absent until created: an optional lens. *)
  left_refused : int;
}

type pair_action = Open | Add_right | Bump [@@deriving message ~name:"pair.action"]

(* The parent's guard on [left]: whatever the route, nothing reaches the left
   counter while the pair is closed. *)
let only_when_open parent Inc _now = parent.opened

let pair : (pair_state, unit, pair_action, never, never) Component.t =
  Component.make ~name:"pair" ~actions:pair_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Open -> Ctx.update ctx (fun s -> { s with opened = true })
      | Add_right -> Ctx.update ctx (fun s -> { s with right = Some 0 })
      | Bump ->
          (* Inline: runs now, and the parent learns the outcome. *)
          if Ctx.reduce_child ctx left Inc = Refused then
            Ctx.update ctx (fun s -> { s with left_refused = s.left_refused + 1 });
          (* Queued: runs after this reducer, as its own execution. *)
          Ctx.to_child ctx right Inc)
    ~register:(fun r ->
      Registrar.child r left
        ~lens:(Lens.field ~get:(fun s -> s.left) ~set:(fun s left -> { s with left }))
        ~guard:only_when_open ();
      Registrar.child r right
        ~lens:
          (Lens.opt ~get:(fun s -> s.right) ~set:(fun s n -> { s with right = Some n }))
        ())
    ()
```

<!-- snippet: pair-run -->
```ocaml
let () =
  let state = { opened = false; left = 0; right = None; left_refused = 0 } in
  let store, _sink = Store.start_exn pair ~state ~services:() in
  let pair_m = Store.mount_exn store (Path.root pair) in
  let left_m = Store.mount_exn store (Path.child (Path.root pair) left) in
  let right_m = Store.mount_exn store (Path.child (Path.root pair) right) in
  (* Closed, and no right counter yet: both calls are refused. *)
  ignore (Store.dispatch_exn store pair_m Bump);
  let s = Store.state store in
  assert ((s.left, s.right, s.left_refused) = (0, None, 1));
  (* The guard holds on every route, not only through the parent's reducer. *)
  assert (Store.dispatch_to store left_m Inc = Ok Disabled);
  List.iter (fun a -> ignore (Store.dispatch_exn store pair_m a)) [ Open; Add_right; Bump ];
  ignore (Store.dispatch_exn store right_m Inc);
  let s = Store.state store in
  assert ((s.left, s.right) = (1, Some 2));
  (* One component, two mounts, two identities. *)
  assert (Protocol.path_of (Store.protocol store) left_m = "/left");
  assert (Protocol.instance_of left_m <> Protocol.instance_of right_m)
```

<!-- snippet: duplicate-answer -->
```ocaml
let () =
  let shop = open_ () in
  ignore (Store.dispatch_exn shop.store shop.payments_mount (Charge { amount = 5 }));
  let request = fst (List.hd (Pending.bindings (Store.state shop.store).payments.pending)) in
  (* A duplicate answer arrives from outside, for the same request. *)
  assert (Sink.try_send_to shop.sink shop.payments_mount charged_message { request; approved = true } = Ok ());
  ignore (Store.drain_events shop.store 16);
  (* The gateway's answer and the duplicate both ran; only the first matched. *)
  assert ((Store.state shop.store).payments.paid = [ 5 ])
```

<!-- snippet: send-unadapted -->
```ocaml
let () =
  let shop = open_ () in
  let request = { Request_id.session = 1; execution = 1; index = 0 } in
  match Sink.try_send_to shop.sink shop.cart_mount charged_message { request; approved = true } with
  | Error (Unregistered _) -> ()
  | _ -> assert false
```

<!-- snippet: wallet -->
<!-- $MDX file=examples/addressing_wallet.ml -->
```ocaml
type sender_state = { sent : int list; refused : int list }

(* [affordable] is context: the parent read it, the child cannot. *)
type send = { amount : int; affordable : bool }
[@@deriving message ~name:"sender.action" ~kind:"sender.send"]

type spent = Spent of int [@@deriving message ~name:"sender.output"]

(* Pays out; the balance it must not exceed is kept by its parent. *)
let sender : (sender_state, unit, send, never, spent) Component.t =
  Component.make ~name:"sender" ~actions:send_message ~effects:Message.never
    ~outputs:spent_message
    ~reduce:(fun ctx { amount; affordable } _ ->
      if not affordable then
        Ctx.update ctx (fun s -> { s with refused = s.refused @ [ amount ] })
      else (
        Ctx.update ctx (fun s -> { s with sent = s.sent @ [ amount ] });
        Ctx.emit_output ctx (Spent amount)))
    ()

type wallet_state = { balance : int; sender : sender_state }

type wallet_action = Pay of int | Debit of int
[@@deriving message ~name:"wallet.action"]

let sender_slot = Child.make "sender" sender

let wallet : (wallet_state, unit, wallet_action, never, never) Component.t =
  Component.make ~name:"wallet" ~actions:wallet_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Pay amount ->
          (* The parent reads what the child cannot, and passes it on. *)
          let affordable = (Ctx.state ctx).balance >= amount in
          ignore (Ctx.reduce_child ctx sender_slot { amount; affordable })
      | Debit amount -> Ctx.update ctx (fun s -> { s with balance = s.balance - amount }))
    ~register:(fun r ->
      Registrar.child r sender_slot
        ~lens:
          (Lens.field ~get:(fun s -> s.sender) ~set:(fun s sender -> { s with sender }))
          (* The child's request, as the parent's Action. *)
        ~on_output:(fun _ (Spent amount) port -> Port.emit_action port (Debit amount))
        ())
    ()
```

<!-- snippet: wallet-run -->
```ocaml
let () =
  let state = { balance = 10; sender = { sent = []; refused = [] } } in
  let store, _sink = Store.start_exn wallet ~state ~services:() in
  let m = Store.mount_exn store (Path.root wallet) in
  ignore (Store.dispatch_exn store m (Pay 7));
  ignore (Store.dispatch_exn store m (Pay 7)) (* 3 left *);
  let s = Store.state store in
  assert (s.sender.sent = [ 7 ] && s.sender.refused = [ 7 ] && s.balance = 3)
```
