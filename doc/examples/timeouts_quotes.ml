(* How long to wait for an answer, in nanoseconds, and how many times to ask. *)
let quote_timeout = 100_000_000
let quote_attempts = 2

(* A supplier's answer. *)
type quoted = Quoted of { request : Request_id.t; price : int }
[@@deriving message ~name:"quote.answer"]

(* Anything that can quote a price and answer later, from any thread. *)
type supplier = { ask : Request_id.t -> string -> quoted reply -> unit }
type quote_services = { supplier : supplier; timer : Timer.t }

(* What an answer needs: the item asked about, and which attempt this is. *)
type quote_attempt = { item : string; attempt : int }

type quote_state = {
  waiting : quote_attempt Pending.t;
  prices : (string * int) list;
  gave_up : string list;
  stale : int;  (** Answers and timers for requests no longer pending. *)
}

type quote_action =
  | Ask_quote of { item : string }
  | Quote_answered of { request : Request_id.t; price : int }
  | Quote_timed_out of { request : Request_id.t }
[@@deriving message ~name:"quote.action"]

type quote_effect =
  | Request_quote of { request : Request_id.t; item : string }
      (** Sends the request to the supplier. *)
  | Arm_quote_timer of { request : Request_id.t }
      (** Arms a timer pinned to the same request. *)
[@@deriving message ~name:"quote.effect"]

(* Every attempt is a new request: an answer to an earlier one is stale. *)
let ask_again ctx item attempt =
  let request = Ctx.new_request ctx in
  Ctx.update ctx (fun s ->
      { s with waiting = Pending.add request { item; attempt } s.waiting });
  Ctx.emit_effect ctx (Request_quote { request; item });
  Ctx.emit_effect ctx (Arm_quote_timer { request })

let quotes : (quote_state, quote_services, quote_action, quote_effect, never) Component.t
    =
  Component.make ~name:"quotes" ~actions:quote_action_message
    ~effects:quote_effect_message ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      let s = Ctx.state ctx in
      match action with
      | Ask_quote { item } -> ask_again ctx item 1
      | Quote_answered { request; price } -> (
          match Pending.take request s.waiting with
          | Some ({ item; _ }, waiting) ->
              Ctx.set_state ctx { s with waiting; prices = s.prices @ [ (item, price) ] }
          (* Too late: it timed out, and was asked again. *)
          | None -> Ctx.set_state ctx { s with stale = s.stale + 1 })
      | Quote_timed_out { request } -> (
          match Pending.take request s.waiting with
          | Some ({ item; attempt }, waiting) when attempt < quote_attempts ->
              Ctx.set_state ctx { s with waiting };
              ask_again ctx item (attempt + 1)
          | Some ({ item; _ }, waiting) ->
              Ctx.set_state ctx { s with waiting; gave_up = s.gave_up @ [ item ] }
          (* Answered in time: the timer is stale. *)
          | None -> Ctx.set_state ctx { s with stale = s.stale + 1 }))
    ~handle:(fun ctx eff _ ->
      let services = Effect_ctx.service ctx in
      match eff with
      | Request_quote { request; item } ->
          services.supplier.ask request item (Effect_ctx.reply ctx quoted_message)
      | Arm_quote_timer { request } ->
          let reply = Effect_ctx.reply ctx Timer.fired in
          Timer.after services.timer ~delay:quote_timeout reply request)
    ~register:(fun r ->
      Registrar.event r quoted_message (fun (Quoted { request; price }) port ->
          Port.emit_action port (Quote_answered { request; price }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Quote_timed_out { request = fired.request })))
    ()
