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
