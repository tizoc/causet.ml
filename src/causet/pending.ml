(* The requests a component waits on: a map from request to its value and
   deadline. Documented in pending.mli. *)

module M = Map.Make (Ids.Request_id)

type 'a entry = { value : 'a; deadline : Ids.timestamp option }
type 'a t = 'a entry M.t

let empty = M.empty
let add ?deadline request value t = M.add request { value; deadline } t

let take request t =
  match M.find_opt request t with
  | Some e -> Some (e.value, M.remove request t)
  | None -> None

let cancel request t = M.remove request t
let retain keep t = M.filter (fun request e -> keep request e.value) t
let clear _ = M.empty

let expire now t =
  let due =
    M.fold
      (fun request e acc ->
        match e.deadline with
        | Some d when d <= now -> (d, request, e.value) :: acc
        | _ -> acc)
      t []
  in
  let due =
    List.sort
      (fun (d, r, _) (d', r', _) ->
        match Int.compare d d' with 0 -> Ids.Request_id.compare r r' | c -> c)
      due
  in
  ( List.map (fun (_, r, v) -> (r, v)) due,
    List.fold_left (fun t (_, r, _) -> M.remove r t) t due )

let next_deadline t =
  M.fold
    (fun _ e acc ->
      match (e.deadline, acc) with
      | Some d, Some a -> Some (min d a)
      | Some d, None -> Some d
      | None, acc -> acc)
    t None

let deadline request t = Option.bind (M.find_opt request t) (fun e -> e.deadline)
let mem = M.mem
let find_opt request t = Option.map (fun e -> e.value) (M.find_opt request t)

let update request f t =
  M.update request (Option.map (fun e -> { e with value = f e.value })) t

let cardinal = M.cardinal
let is_empty = M.is_empty
let bindings t = List.map (fun (r, e) -> (r, e.value)) (M.bindings t)
let values t = List.map snd (bindings t)
let equal eq a b = M.equal (fun x y -> eq x.value y.value && x.deadline = y.deadline) a b

let pp pp_value f t =
  let entry f e =
    Debug.record f "Entry"
      [
        ("value", fun f -> pp_value f e.value);
        ("deadline", fun f -> Debug.option Debug.int f e.deadline);
      ]
  in
  Format.fprintf f "Pending { entries: {%a} }"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
       (fun f (r, e) -> Format.fprintf f "%a: %a" Ids.Request_id.pp r entry e))
    (M.bindings t)
