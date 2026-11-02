The causet guide's OCaml examples for its authority page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: shop-opens -->
```ocaml
open Causet
open Causet_guide.Shop
```

<!-- snippet: harness -->
```ocaml
let () =
  let harness = Harness.create () in
  let state = { pending = Pending.empty; paid = []; declined = [] } in
  let state, emissions = Harness.reduce harness payments state (Charge { amount = 30 }) in
  (* It remembered the request, then emitted the Effect that carries it. *)
  match (Pending.bindings state.pending, emissions) with
  | [ (request, 30) ], [ Emit_effect (Charge_effect { request = r; amount = 30 }) ] ->
      assert (Request_id.equal r request)
  | _ -> assert false
```

<!-- snippet: state-after-emit -->
```ocaml
# let late =
    Component.make ~name:"late" ~actions:cart_action_message ~effects:Message.never
      ~outputs:cart_output_message
      ~reduce:(fun ctx _ _ ->
        Ctx.emit_output ctx (Checked_out { total = 0 });
        Ctx.set_state ctx { items = []; total = 0 })
      ()
  in
  Harness.reduce (Harness.create ()) late { items = []; total = 0 } Checkout;;
Exception:
Invalid_argument "causet: set_state after the reducer started emitting".
```

<!-- snippet: emit-own-action -->
```ocaml
# let wrong =
    Component.make ~name:"wrong" ~actions:cart_action_message ~effects:Message.never
      ~outputs:Message.never
      ~reduce:(fun ctx _ _ -> Ctx.emit_action ctx (Charge { amount = 1 }))
      ()
  in
  ignore wrong;;
Line 4, characters 52-58:
Error: This variant expression is expected to have type cart_action
       There is no constructor Charge within type cart_action
```

<!-- snippet: is-enabled -->
```ocaml
let () = assert (not (Harness.is_enabled (Harness.create ()) cart { items = []; total = 0 } Checkout))
```

<!-- snippet: handler-reads-state -->
```ocaml
# fun ctx -> Effect_ctx.set_state ctx empty;;
Line 1, characters 12-32:
Error: Unbound value Effect_ctx.set_state
```

<!-- snippet: port-emits-actions -->
```ocaml
# fun port (_ : charged) -> Port.emit_effect port (Charge_effect { request = Obj.magic 0; amount = 1 });;
Line 1, characters 27-43:
Error: Unbound value Port.emit_effect
```

<!-- snippet: dispatch-from-outside -->
```ocaml
let () =
  let shop = open_ () in
  assert (Store.dispatch_to shop.store shop.payments_mount (Charge { amount = 7 }) = Ok Executed)
```
