type stamp = Now [@@deriving message ~name:"stamp.action"]

(* Keeps the time of every Action it reduced. *)
let stamps : (int list, unit, stamp, never, never) Component.t =
  Component.make ~name:"stamps" ~actions:stamp_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Now (meta : Meta.t) ->
      (* The execution's time, never the system clock's. *)
      Ctx.update ctx (fun times -> times @ [ meta.time ]))
    ()
