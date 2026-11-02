(* A field of type [cup_id] is printed by [pp_cup_id]: [@@deriving debug]
   makes it. *)
type cup_id = Cup_id of int [@@unboxed] [@@deriving debug]

(* A field of type [Millilitres.t] by [Millilitres.pp], written here with
   [Debug], which prints as Rust's [Debug] does. *)
module Millilitres = struct
  type t = int

  let pp f n = Debug.tuple f "Millilitres" [ (fun f -> Debug.int f n) ]
end

type tea_action =
  | Pour of { cup : cup_id; amount : Millilitres.t }
  | Rinse of cup_id list
[@@deriving message ~name:"tea.action"]

(* The Effect that carries out a Pour is named apart, as one OCaml module
   needs, and keeps the Rust port's kind and printed name. *)
type tea_effect =
  | Pour_effect of { cup : cup_id } [@kind "tea.pour"] [@debug_name "Pour"]
[@@deriving message ~name:"tea.effect"]

let () =
  let show pp v = Debug.to_string pp v in
  assert (
    show pp_tea_action (Pour { cup = Cup_id 1; amount = 250 })
    = "Pour { cup: CupId(1), amount: Millilitres(250) }");
  assert (
    show pp_tea_action (Rinse [ Cup_id 1; Cup_id 2 ]) = "Rinse([CupId(1), CupId(2)])");
  assert (
    show tea_effect_message.pp (Pour_effect { cup = Cup_id 1 }) = "Pour { cup: CupId(1) }")
