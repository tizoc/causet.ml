(* The example application the guide's examples run: a shop whose cart
   checks out through a payment component. Every piece of causet appears
   once.

   {v
   /            shop_root  root: State is the whole shop State, Service the whole Services
   /cart        cart       no Effects; reports Checked_out to its parent as an Output
   /payments    payments   charges through the gateway Service, answered by a Charged Event
   v}

   A checkout flows through every category:

   {v
   Checkout -> cart reducer -> Output Checked_out
     -> shop_root's lowerer -> Pay -> shop_root reducer -> to_child payments Charge
     -> payments reducer -> Effect Charge -> gateway -> Event Charged
     -> drain_events -> payments adapter -> Settled -> payments reducer
   v} *)

open Causet

(* ---------------------------------------------------------------------- *)
(* Cart: a leaf with no Effects. *)

type cart_state = { items : string list; total : int }

type cart_action = Add of { item : string; price : int } | Checkout
[@@deriving message ~name:"cart.action"]

type cart_output = Checked_out of { total : int }
[@@deriving message ~name:"cart.output"]

let cart =
  Component.make ~name:"cart" ~actions:cart_action_message ~effects:Message.never
    ~outputs:cart_output_message (* A checkout of an empty cart is refused. *)
    ~is_enabled:(fun state action _ ->
      match action with Checkout -> state.items <> [] | Add _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Add { item; price } ->
          Ctx.update ctx (fun c ->
              { items = c.items @ [ item ]; total = c.total + price })
      | Checkout ->
          let total = (Ctx.state ctx).total in
          Ctx.set_state ctx { items = []; total = 0 };
          Ctx.emit_output ctx (Checked_out { total }))
    ()

(* ---------------------------------------------------------------------- *)
(* Payments: Effects, a Service and an Event. *)

[@@@part "payments"]

type payments_state = {
  pending : int Pending.t;
      (** Charges sent and not answered yet, by request: the first answer to each takes
          it, and any after it is stale. *)
  paid : int list;
  declined : int list;
}

type payments_action =
  | Charge of { amount : int }
  | Settled of { request : Request_id.t; approved : bool }
[@@deriving message ~name:"payments.action"]

(* Named apart from the Action it carries out; its kind is the Rust port's. *)
type payments_effect =
  | Charge_effect of { request : Request_id.t; amount : int } [@kind "payments.charge"]
[@@deriving message ~name:"payments.effect"]

(* The gateway's answer, published through the Event bus. *)
type charged = { request : Request_id.t; approved : bool }
[@@deriving message ~name:"payments.charged"]

(* The payments capability: anything that can charge and answer later. *)
type gateway = { charge : Request_id.t -> int -> charged reply -> unit }

(* Approves charges up to a limit, answering at once on the Store's thread.
   A real gateway would answer from a worker thread through the same reply. *)
let approve_up_to limit =
  {
    charge =
      (fun request amount reply ->
        match Reply.try_send reply { request; approved = amount <= limit } with
        | Ok () -> ()
        | Error _ -> failwith "the Store lane is never full");
  }

let payments =
  Component.make ~name:"payments" ~actions:payments_action_message
    ~effects:payments_effect_message ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Charge { amount } ->
          (* Made during mutation, so it can be both remembered and sent. *)
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s ->
              { s with pending = Pending.add request amount s.pending });
          Ctx.emit_effect ctx (Charge_effect { request; amount })
      | Settled { request; approved } -> (
          let s = Ctx.state ctx in
          (* An answer to no pending request is stale: ignored. *)
          match Pending.take request s.pending with
          | None -> ()
          | Some (amount, pending) ->
              Ctx.set_state ctx
                (if approved then { s with pending; paid = s.paid @ [ amount ] }
                 else { s with pending; declined = s.declined @ [ amount ] })))
    ~handle:(fun ctx (Charge_effect { request; amount }) _ ->
      let reply = Effect_ctx.reply ctx charged_message in
      (Effect_ctx.service ctx).charge request amount reply)
    ~register:(fun r ->
      Registrar.event r charged_message (fun (ev : charged) port ->
          Port.emit_action port (Settled { request = ev.request; approved = ev.approved })))
    ()

[@@@part "rest"]

(* ---------------------------------------------------------------------- *)
(* Shop: the root. *)

type shop_state = { cart : cart_state; payments : payments_state; orders : int }
type shop_services = { gateway : gateway }
type shop_action = Pay of { total : int } [@@deriving message ~name:"shop.action"]

let cart_slot = Child.make "cart" cart
let payments_slot = Child.make "payments" payments

let shop_root =
  Component.make ~name:"shop" ~actions:shop_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Pay { total }) _ ->
      Ctx.update ctx (fun s -> { s with orders = s.orders + 1 });
      Ctx.to_child ctx payments_slot (Charge { amount = total }))
    ~register:(fun r ->
      Registrar.child r cart_slot
        ~lens:(Lens.field ~get:(fun s -> s.cart) ~set:(fun s cart -> { s with cart }))
          (* The parent decides what a child's Output means. *)
        ~on_output:(fun _ (Checked_out { total }) port ->
          Port.emit_action port (Pay { total }))
        ();
      Registrar.child r payments_slot
        ~lens:
          (Lens.field
             ~get:(fun s -> s.payments)
             ~set:(fun s payments -> { s with payments }))
        ~service:(fun svc -> svc.gateway)
        ())
    ()

let empty =
  {
    cart = { items = []; total = 0 };
    payments = { pending = Pending.empty; paid = []; declined = [] };
    orders = 0;
  }

(* The frozen Protocol: built once, shared by every Store of the shop. *)
let protocol () =
  let reg = Registry.create () in
  Registry.root reg shop_root;
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

(* A running shop: its Store, the bus's sending side and its two mounts. *)
type shop = {
  store : (shop_state, shop_services) Store.t;
  sink : Sink.t;
  cart_mount : (cart_state, unit, cart_action, never, cart_output) mount;
  payments_mount :
    (payments_state, gateway, payments_action, payments_effect, never) mount;
}

(* A shop whose gateway approves charges up to 100. *)
let open_ ?clock () =
  let store, sink =
    Store.start_exn ?clock shop_root ~state:empty
      ~services:{ gateway = approve_up_to 100 }
  in
  {
    store;
    sink;
    cart_mount = Store.mount_exn store (Path.child (Path.root shop_root) cart_slot);
    payments_mount =
      Store.mount_exn store (Path.child (Path.root shop_root) payments_slot);
  }
