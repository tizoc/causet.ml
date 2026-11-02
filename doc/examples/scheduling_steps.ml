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
