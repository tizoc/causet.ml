(** The file dump: records as JSON lines, rotated by size. A dump shared by threads writes
    each line whole: a record is encoded first, then rotation and its single write happen
    under one lock. *)

type t

val create : Config.Dump.t -> t
(** A dump configured by [config]; the directory is created on the first write. *)

val dump : t -> Report.Record.t -> unit
(** Appends [record] as one JSON line, first rotating the file if it has reached
    [max_file_size_bytes] (a rotated file gets [-<timestamp_ms>] before its extension).
    Writes nothing while the dump is disabled or [dir] is empty. Raises [Sys_error] or
    [Unix.Unix_error] on failure. *)
