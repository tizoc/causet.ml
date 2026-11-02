type fragile = Fine | Break [@@deriving message ~name:"fragile.action"]

let glass : (int, unit, fragile, never, never) Component.t =
  Component.make ~name:"glass" ~actions:fragile_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (( + ) 1);
      match action with Break -> failwith "broke" | Fine -> ())
    ()
