(* Printers in Rust's [Debug] form, for the [message] deriver and for
   anything that must print as the Rust port does: [Add { item: "tea",
   price: 4 }], [Some(3)], [[1, 2]], strings quoted and escaped. *)

let int f n = Format.fprintf f "%d" n
let int32 f n = Format.fprintf f "%ld" n
let int64 f n = Format.fprintf f "%Ld" n
let bool f b = Format.pp_print_bool f b
let unit f () = Format.pp_print_string f "()"

(* Shortest digits that read back as the same float, with [.0] on whole
   numbers, as Rust prints an [f64]. *)
let float f x =
  if Float.is_integer x && Float.abs x < 1e16 then Format.fprintf f "%.1f" x
  else
    let rec shortest p =
      let s = Printf.sprintf "%.*g" p x in
      if p >= 17 || float_of_string s = x then s else shortest (p + 1)
    in
    Format.pp_print_string f (shortest 1)

let escape_char c =
  match c with
  | '"' -> "\\\""
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\000' -> "\\0"
  | c when Char.code c < 0x20 || Char.code c = 0x7f ->
      Printf.sprintf "\\u{%x}" (Char.code c)
  | c -> String.make 1 c

let string f s =
  Format.pp_print_char f '"';
  String.iter
    (fun c ->
      if c = '\'' then Format.pp_print_char f c
      else Format.pp_print_string f (escape_char c))
    s;
  Format.pp_print_char f '"'

let char f c =
  Format.pp_print_char f '\'';
  Format.pp_print_string f
    (if c = '\'' then "\\'" else if c = '"' then "\"" else escape_char c);
  Format.pp_print_char f '\''

let comma f () = Format.pp_print_string f ", "
let list pp f l = Format.fprintf f "[%a]" (Format.pp_print_list ~pp_sep:comma pp) l
let array pp f a = list pp f (Array.to_list a)

(* As Rust prints a [Vec<u8>]: [[1, 2]]. *)
let bytes f b = list (fun f c -> int f (Char.code c)) f (List.of_seq (Bytes.to_seq b))

let option pp f = function
  | None -> Format.pp_print_string f "None"
  | Some x -> Format.fprintf f "Some(%a)" pp x

(* A value nothing can print, such as a function. *)
let opaque f _ = Format.pp_print_string f "_"

(* [Name(a, b)], each item printed by its thunk. *)
let tuple f name items =
  Format.fprintf f "%s(%a)" name
    (Format.pp_print_list ~pp_sep:comma (fun f item -> item f))
    items

(* [Name { a: 1, b: 2 }]; [Name] alone when there are no fields. *)
let record f name fields =
  match fields with
  | [] -> Format.pp_print_string f name
  | _ ->
      Format.fprintf f "%s { %a }" name
        (Format.pp_print_list ~pp_sep:comma (fun f (label, item) ->
             Format.fprintf f "%s: %t" label item))
        fields

let to_string pp v = Format.asprintf "%a" pp v
