open Kernel_types

(* ------------------------------------------------------------------------ *)
(* State and Service projections *)

let lens_of proto m =
  match proto.mounts.(m).lens with Some l -> l | None -> invalid_arg "causet: no lens"

let state_get proto root mount =
  let chain = proto.mounts.(mount).chain in
  let rec go cur i =
    if i = Array.length chain then Some cur
    else
      match (lens_of proto chain.(i)).lget cur with None -> None | Some c -> go c (i + 1)
  in
  go root 0

let state_set proto root mount v =
  let chain = proto.mounts.(mount).chain in
  let rec go cur i =
    if i = Array.length chain then v
    else
      let l = lens_of proto chain.(i) in
      match l.lget cur with
      | None -> invalid_arg "causet: a State written back is absent"
      | Some c -> l.lset cur (go c (i + 1))
  in
  go root 0

let service_get proto root mount =
  let chain = proto.mounts.(mount).chain in
  let rec go cur i =
    if i = Array.length chain then Some cur
    else
      match proto.mounts.(chain.(i)).service with
      | None -> None
      | Some f -> go (f cur) (i + 1)
  in
  go root 0
