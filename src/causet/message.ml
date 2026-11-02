(* A message type: its name, how to tell a value's kind, how logging records
   it, and a type witness. A value of ['a t] describes every value of ['a];
   it is the OCaml port's form of the Rust derives. *)

type level = Trace | Debug | Info | Warn | Error

type 'a t = {
  name : string;  (** The type's name, as logs, stats and observers show it. *)
  id : 'a Type.Id.t;
  kind : 'a -> string;  (** A value's kind, such as [cart.add]. *)
  level : 'a -> level;  (** The level logging records a value at. *)
  fields : 'a -> (string * string) list;  (** The fields logging records. *)
  pp : Format.formatter -> 'a -> unit;  (** A value's debug form. *)
}

let make ?(level = fun _ -> Debug) ?(fields = fun _ -> [])
    ?(pp = fun f _ -> Format.pp_print_string f "_") ~name ~kind () =
  { name; id = Type.Id.make (); kind; level; fields; pp }

(* A message type of one kind, its name. *)
let single ?level ?fields ?pp name =
  make ?level ?fields ?pp ~name ~kind:(fun _ -> name) ()

(* The type with no values: the Effects or Outputs of a component that has
   none. *)
type never = |

let absurd : never -> 'a = function _ -> .
let never : never t = make ~name:"never" ~kind:absurd ~pp:(fun _ v -> absurd v) ()
let same a b = Type.Id.uid a.id = Type.Id.uid b.id
let is_never m = same m never

(* A message of some type, with its description: what queues, buses and
   observers carry. *)
type payload = Payload : 'a t * 'a -> payload

let payload m v = Payload (m, v)

let get (type a) (m : a t) (Payload (m', v)) : a option =
  match Type.Id.provably_equal m.id m'.id with Some Type.Equal -> Some v | None -> None

let get_exn m p =
  match get m p with Some v -> v | None -> invalid_arg ("causet: not a " ^ m.name)

let kind (Payload (m, v)) = m.kind v
let name (Payload (m, _)) = m.name
let uid (Payload (m, _)) = Type.Id.uid m.id
let log_level (Payload (m, v)) = m.level v
let log_fields (Payload (m, v)) = m.fields v
let pp_payload f (Payload (m, v)) = m.pp f v
