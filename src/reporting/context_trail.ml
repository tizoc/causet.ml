(* A bounded ring of recent context entries, for reports to include the
   last few: the oldest is overwritten once it is full. *)

type 'a t = { buf : 'a option array; mutable head : int; mutable len : int }

let create capacity = { buf = Array.make capacity None; head = 0; len = 0 }
let capacity t = Array.length t.buf
let length t = t.len
let is_empty t = t.len = 0

let clear t =
  Array.fill t.buf 0 (Array.length t.buf) None;
  t.head <- 0;
  t.len <- 0

(* Pushes [item], overwriting the oldest if full; nothing when the capacity
   is zero. *)
let push t item =
  let cap = Array.length t.buf in
  if cap > 0 then (
    t.buf.(t.head) <- Some item;
    t.head <- (t.head + 1) mod cap;
    if t.len < cap then t.len <- t.len + 1)

(* The first entry, in slot order, that [pred] accepts. *)
let find t pred =
  Array.find_map (function Some x when pred x -> Some x | _ -> None) t.buf

(* The first entry [pred] accepts, or a new one from [make], pushed: [None]
   only when the capacity is zero. Entries are values; to update one in
   place, make them mutable records. *)
let get_or_push t pred make =
  match find t pred with
  | Some x -> Some x
  | None ->
      if capacity t = 0 then None
      else
        let x = make () in
        push t x;
        Some x

(* The most recent [max] entries, oldest first. *)
let recent t max =
  let cap = Array.length t.buf in
  let count = min t.len max in
  List.init count (fun i ->
      match t.buf.((t.head + cap - count + i) mod cap) with
      | Some x -> x
      | None -> assert false)

(* Every entry, oldest first. *)
let to_list t = recent t t.len
let iter f t = List.iter f (to_list t)
