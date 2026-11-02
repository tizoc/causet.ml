(** BLAKE3, the 32-byte hash of an input, bit for bit the reference algorithm: the Rust
    port samples, dedupes and picks reservoir samples by it, so the two ports keep the
    same reports. *)

val hash : string -> string
(** The 32-byte digest of [input]. *)

val u64_prefix : string -> int64
(** A digest's first eight bytes, little-endian, as Rust reads a [u64] of them. *)

val to_hex : string -> string

(** {1 Incremental hashing}

    A hasher takes the input in pieces and gives the digest of their concatenation, the
    same as {!hash} of it. It allocates nothing per piece, and after a digest it is ready
    for the next input, so one hasher serves any number of hashes. *)

type t

val create : unit -> t

val reset : t -> unit
(** Drops the input so far. *)

val add_char : t -> char -> unit
val add_string : t -> string -> unit
val add_substring : t -> string -> int -> int -> unit

val add_int64_le : t -> int64 -> unit
(** The eight bytes of the integer, little-endian. *)

val add_int_le : t -> int -> unit
(** As [add_int64_le] of [Int64.of_int n]: a negative number sign-extended. *)

val add_float_bits_le : t -> float -> unit
(** As [add_int64_le] of [Int64.bits_of_float x]. *)

val digest : t -> string
(** The digest of the input so far; the hasher starts again empty. *)

val digest_into : t -> Bytes.t -> int -> unit
(** Writes the digest at the offset, as {!digest} returns it. *)

val digest_u64_prefix : t -> int64
(** The digest's first eight bytes, as {!u64_prefix} of {!digest}, without making the
    digest. *)
