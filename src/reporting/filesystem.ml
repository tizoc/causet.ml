(* The file dump: records as JSON lines, rotated by size. A dump shared by
   threads writes each line whole: a record is encoded first, then rotation
   and its single write happen under one lock. *)

type t = { config : Config.Dump.t; lock : Mutex.t; mutable rotations : int }

(* A dump configured by [config]; the directory is created on the first
   write. *)
let create config = { config; lock = Mutex.create (); rotations = 0 }

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let size path = try (Unix.stat path).st_size with Unix.Unix_error _ -> 0

let rotated_path t timestamp_ms =
  let { Config.Dump.dir; filename; _ } = t.config in
  let stem, ext =
    match String.rindex_opt filename '.' with
    | Some i ->
        ( String.sub filename 0 i,
          String.sub filename (i + 1) (String.length filename - i - 1) )
    | None -> (filename, "jsonl")
  in
  let counter = t.rotations in
  t.rotations <- counter + 1;
  let rec find counter tries =
    if tries = 0 then
      Filename.concat dir
        (Printf.sprintf "%s-%d-%.0f.%s" stem timestamp_ms
           (Unix.gettimeofday () *. 1e9)
           ext)
    else
      let suffix =
        if counter = 0 then string_of_int timestamp_ms
        else Printf.sprintf "%d-%d" timestamp_ms counter
      in
      let path = Filename.concat dir (Printf.sprintf "%s-%s.%s" stem suffix ext) in
      if Sys.file_exists path then find (counter + 1) (tries - 1) else path
  in
  find counter 1024

let maybe_rotate t path timestamp_ms =
  let limit = t.config.max_file_size_bytes in
  if limit > 0 && size path >= limit then
    let rotated = rotated_path t timestamp_ms in
    if rotated <> path then Sys.rename path rotated

(* Appends [record] as one JSON line, first rotating the file if it has
   reached [max_file_size_bytes]. Writes nothing while the dump is disabled
   or [dir] is empty. Raises [Sys_error] or [Unix.Unix_error] on failure. *)
let dump t (record : Report.Record.t) =
  if t.config.enabled && t.config.dir <> "" then (
    let line = Buffer.create 1024 in
    Report.Record.write_json line record;
    Buffer.add_char line '\n';
    Mutex.protect t.lock (fun () ->
        mkdir_p t.config.dir;
        let path = Filename.concat t.config.dir t.config.filename in
        maybe_rotate t path record.timestamp_ms;
        Out_channel.with_open_gen [ Open_wronly; Open_append; Open_creat; Open_binary ]
          0o644 path (fun oc -> Out_channel.output_string oc (Buffer.contents line))))
