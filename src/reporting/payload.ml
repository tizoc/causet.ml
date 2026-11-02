(* A report's content: a JSON object of fields, then objects (sections),
   then arrays, written in that order, as the Rust port writes its pooled
   payloads. Integers keep Rust's two kinds, unsigned and signed, since a
   report's hash tells them apart. *)

type value =
  | Null
  | Bool of bool
  | Uint of int  (** Rust's [u64]: never negative. *)
  | Int of int  (** Rust's [i64]. *)
  | Float of float  (** Written as [null] when not finite. *)
  | String of string
  | Raw_json of string  (** Serialized JSON, written as is. *)

type t = {
  fields : (string * value) list;
  sections : (string * t) list;
  arrays : (string * item list) list;
}

and item = Value of value | Object of t | Array of item list

(* ------------------------------------------------------------------------ *)
(* Values *)

let null = Null
let bool b = Bool b

let uint n =
  if n < 0 then invalid_arg "Payload.uint: negative";
  Uint n

let int n = Int n
let float x = Float x
let string s = String s
let raw_json s = Raw_json s

(* [x] as [pp] prints it, as Rust's [display] adapter does. *)
let display pp x = String (Format.asprintf "%a" pp x)

(* [j] serialized, as Rust's [json] adapter does. *)
let json j = Raw_json (Yojson.Safe.to_string j)

(* A float as Rust's [Display] prints it: the shortest digits that read
   back as the same number, never in exponent form, with no [.0] on a
   whole number. *)
let float_to_string x =
  if Float.is_nan x then "NaN"
  else if Float.is_integer x && Float.abs x < 1e15 then
    if x = 0. && 1. /. x < 0. then "-0" else Printf.sprintf "%.0f" x
  else if Float.abs x = Float.infinity then if x > 0. then "inf" else "-inf"
  else
    let rec shortest p =
      let s = Printf.sprintf "%.*e" (p - 1) x in
      if p >= 17 || float_of_string s = x then s else shortest (p + 1)
    in
    let s = shortest 1 in
    let negative = s.[0] = '-' in
    let s = if negative then String.sub s 1 (String.length s - 1) else s in
    let e = String.index s 'e' in
    let mantissa = String.sub s 0 e in
    let exponent = int_of_string (String.sub s (e + 1) (String.length s - e - 1)) in
    let digits = String.concat "" (String.split_on_char '.' mantissa) in
    let n = String.length digits in
    let body =
      if exponent >= 0 then
        if exponent + 1 >= n then digits ^ String.make (exponent + 1 - n) '0'
        else
          String.sub digits 0 (exponent + 1)
          ^ "."
          ^ String.sub digits (exponent + 1) (n - exponent - 1)
      else "0." ^ String.make (-exponent - 1) '0' ^ digits
    in
    if negative then "-" ^ body else body

(* A value as a number, as metric aggregation reads one. *)
let to_float = function
  | Bool b -> Some (if b then 1. else 0.)
  | Uint n | Int n -> Some (float_of_int n)
  | Float x -> Some x
  | Null | String _ | Raw_json _ -> None

(* A value as text, as aggregation groups count it. *)
let to_text = function
  | Null | Raw_json _ -> None
  | Bool b -> Some (string_of_bool b)
  | Uint n | Int n -> Some (string_of_int n)
  | Float x -> Some (float_to_string x)
  | String s -> Some s

(* ------------------------------------------------------------------------ *)
(* Reading *)

(* The first binding of [key], compared as a string: [List.assoc_opt]
   compares polymorphically, which costs a call per element. *)
let rec assoc key = function
  | [] -> None
  | (k, v) :: rest -> if String.equal k key then Some v else assoc key rest

let find_field t key = assoc key t.fields
let find_section t name = assoc name t.sections
let find_array t name = assoc name t.arrays

(* The top-level [event] field, when it is a string. *)
let event_str t =
  let rec go = function
    | [] -> None
    | (k, v) :: rest -> (
        if not (String.equal k "event") then go rest
        else match v with String s -> Some s | _ -> None)
  in
  go t.fields

(* ------------------------------------------------------------------------ *)
(* Building *)

(* A payload being built; its lists are newest first until [finish]. *)
type builder = t

(* An array being built, newest item first. *)
type array_builder = item list

let empty_object = { fields = []; sections = []; arrays = [] }
let empty () = empty_object

(* A payload whose first field, [event], holds [event]. *)
let create event = { empty_object with fields = [ ("event", String event) ] }

let rec finish_object b =
  {
    fields = List.rev b.fields;
    sections = List.rev_map (fun (k, s) -> (k, finish_object s)) b.sections;
    arrays = List.rev_map (fun (k, a) -> (k, finish_items a)) b.arrays;
  }

and finish_items items =
  List.rev_map
    (function
      | Object o -> Object (finish_object o) | Array a -> Array (finish_items a) | v -> v)
    items

(* Appends a field. Keys are not checked for repeats. *)
let field key value b = { b with fields = (key, value) :: b.fields }
let field_opt key value b = match value with Some v -> field key v b | None -> b

(* Adds the object [name], filled by [fill]. *)
let section name fill b = { b with sections = (name, fill empty_object) :: b.sections }

(* Adds the object [name] holding one field per pair. *)
let section_fields name pairs b =
  section name (fun s -> List.fold_left (fun s (k, v) -> field k v s) s pairs) b

(* Adds the object [name] holding, for each [(key, item)], an object [key]
   that [fill] fills from [item]. *)
let section_objects name entries fill b =
  section name
    (fun s ->
      List.fold_left (fun s (k, item) -> section k (fun o -> fill o item) s) s entries)
    b

(* Adds the array [name], filled by [fill]. *)
let array name fill b = { b with arrays = (name, fill []) :: b.arrays }
let push_value v (a : array_builder) = Value v :: a
let push_object fill (a : array_builder) = Object (fill empty_object) :: a
let push_array fill (a : array_builder) = Array (fill []) :: a

let array_values name values b =
  array name (fun a -> List.fold_left (fun a v -> push_value v a) a values) b

(* Adds the array [name] holding one object per item, filled by [fill]. *)
let array_objects name items fill b =
  array name
    (fun a -> List.fold_left (fun a item -> push_object (fun o -> fill o item) a) a items)
    b

(* Ends building. *)
let finish (b : builder) : t = finish_object b

(* Ends building, with [key] as the report's dedupe key. *)
let finish_with_dedupe_key key b = (finish b, key)

(* ------------------------------------------------------------------------ *)
(* JSON *)

let write_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let write_value buf = function
  | Null -> Buffer.add_string buf "null"
  | Bool b -> Buffer.add_string buf (string_of_bool b)
  | Uint n | Int n -> Buffer.add_string buf (string_of_int n)
  | Float x ->
      Buffer.add_string buf (if Float.is_finite x then float_to_string x else "null")
  | String s -> write_string buf s
  | Raw_json s -> Buffer.add_string buf s

let write_key buf first key =
  if not !first then Buffer.add_char buf ',';
  first := false;
  write_string buf key;
  Buffer.add_char buf ':'

let rec write_fields buf first t =
  List.iter
    (fun (k, v) ->
      write_key buf first k;
      write_value buf v)
    t.fields;
  List.iter
    (fun (k, s) ->
      write_key buf first k;
      write_object buf s)
    t.sections;
  List.iter
    (fun (k, a) ->
      write_key buf first k;
      write_items buf a)
    t.arrays

and write_object buf t =
  Buffer.add_char buf '{';
  write_fields buf (ref true) t;
  Buffer.add_char buf '}'

and write_items buf items =
  Buffer.add_char buf '[';
  List.iteri
    (fun i item ->
      if i > 0 then Buffer.add_char buf ',';
      match item with
      | Value v -> write_value buf v
      | Object o -> write_object buf o
      | Array a -> write_items buf a)
    items;
  Buffer.add_char buf ']'

(* Writes the payload as one JSON object; with [run_id], that field first. *)
let write_json ?run_id buf t =
  Buffer.add_char buf '{';
  let first = ref true in
  Option.iter
    (fun id ->
      write_key buf first "run_id";
      write_string buf id)
    run_id;
  write_fields buf first t;
  Buffer.add_char buf '}'

let to_json t =
  let buf = Buffer.create 128 in
  write_json buf t;
  Buffer.contents buf
