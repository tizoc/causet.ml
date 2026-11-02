module String_map = Map.Make (String)

type shelf = {
  titles : string array;
  index : int String_map.t;  (** Positions in [titles], by title. *)
}

(* A helper with no context: the report finds its Store by itself. *)
let take shelf title =
  match String_map.find_opt title shelf.index with
  | None -> (shelf, None)
  | Some at ->
      let shelf = { shelf with index = String_map.remove title shelf.index } in
      if at >= Array.length shelf.titles then (
        Bug.condition __POS__
          ~fields:[ ("title", title); ("at", string_of_int at) ]
          "the index names a title the shelf does not have";
        (shelf, None) (* the fallback: as if it were not there *))
      else (shelf, Some shelf.titles.(at))

type shelf_action = Take of string [@@deriving message ~name:"shelf.action"]

let shelves : (shelf, unit, shelf_action, never, never) Component.t =
  Component.make ~name:"shelves" ~actions:shelf_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Take title) _ -> Ctx.update ctx (fun s -> fst (take s title)))
    ()
