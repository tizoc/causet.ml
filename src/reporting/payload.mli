(** A report's content: a JSON object of fields, then objects (sections), then arrays,
    written in that order, as the Rust port writes its pooled payloads. Integers keep
    Rust's two kinds, unsigned and signed, since a report's hash tells them apart. *)

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
(** A finished payload. *)

and item = Value of value | Object of t | Array of item list

(** {1 Values} *)

val null : value
val bool : bool -> value

val uint : int -> value
(** Raises [Invalid_argument] on a negative number. *)

val int : int -> value
val float : float -> value
val string : string -> value
val raw_json : string -> value

val display : (Format.formatter -> 'a -> unit) -> 'a -> value
(** [x] as [pp] prints it, as Rust's [display] adapter does. *)

val json : Yojson.Safe.t -> value
(** [j] serialized, as Rust's [json] adapter does. *)

val float_to_string : float -> string
(** A float as Rust's [Display] prints it: the shortest digits that read back as the same
    number, never in exponent form, with no [.0] on a whole number. *)

val to_float : value -> float option
(** A value as a number, as metric aggregation reads one: a boolean is 0 or 1, text is
    none. *)

val to_text : value -> string option
(** A value as text, as aggregation groups count it; [None] for [null] and raw JSON. *)

(** {1 Reading} *)

val find_field : t -> string -> value option
val find_section : t -> string -> t option
val find_array : t -> string -> item list option

val event_str : t -> string option
(** The top-level [event] field, when it is a string. *)

(** {1 Building} *)

type builder
(** A payload being built. *)

type array_builder
(** An array being built. *)

val empty : unit -> builder

val create : string -> builder
(** A payload whose first field, [event], holds [event]. *)

val field : string -> value -> builder -> builder
(** Appends a field. Keys are not checked for repeats. *)

val field_opt : string -> value option -> builder -> builder
(** Appends the field when there is a value. *)

val section : string -> (builder -> builder) -> builder -> builder
(** Adds the object [name], filled by [fill]. *)

val section_fields : string -> (string * value) list -> builder -> builder
(** Adds the object [name] holding one field per pair. *)

val section_objects :
  string -> (string * 'a) list -> (builder -> 'a -> builder) -> builder -> builder
(** Adds the object [name] holding, for each [(key, item)], an object [key] that [fill]
    fills from [item]. *)

val array : string -> (array_builder -> array_builder) -> builder -> builder
(** Adds the array [name], filled by [fill]. *)

val push_value : value -> array_builder -> array_builder
val push_object : (builder -> builder) -> array_builder -> array_builder
val push_array : (array_builder -> array_builder) -> array_builder -> array_builder

val array_values : string -> value list -> builder -> builder
(** Adds the array [name] holding [values]. *)

val array_objects : string -> 'a list -> (builder -> 'a -> builder) -> builder -> builder
(** Adds the array [name] holding one object per item, filled by [fill]. *)

val finish : builder -> t
(** Ends building. *)

val finish_with_dedupe_key : string -> builder -> t * string
(** Ends building, with [key] as the report's dedupe key. *)

(** {1 JSON} *)

val write_json : ?run_id:string -> Buffer.t -> t -> unit
(** Writes the payload as one JSON object; with [run_id], that field first. *)

val to_json : t -> string

val write_string : Buffer.t -> string -> unit
(** A JSON string, escaped as the Rust port escapes it. *)
