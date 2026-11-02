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
