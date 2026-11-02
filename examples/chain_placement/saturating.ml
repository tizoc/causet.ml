(* Saturating arithmetic on [int], as the Rust port's on [i64]: a sum or
   difference past the bounds is the bound. OCaml's [int] has 63 bits, so
   the bounds are [min_int] and [max_int], not i64's. *)

let add a b =
  let s = a + b in
  if a >= 0 && b >= 0 && s < 0 then max_int
  else if a < 0 && b < 0 && s >= 0 then min_int
  else s

let sub a b =
  let d = a - b in
  if a >= 0 && b < 0 && d < 0 then max_int
  else if a < 0 && b >= 0 && d >= 0 then min_int
  else d
