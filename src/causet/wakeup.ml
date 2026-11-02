(* Standard-library socket wakeups, without select's Unix descriptor-number
   limit. The writer never blocks; callers coalesce and serialize writes. *)
type t = { reader : Unix.file_descr; writer : Unix.file_descr; byte : bytes }

let close t =
  Fun.protect ~finally:(fun () -> Unix.close t.writer) (fun () -> Unix.close t.reader)

let create () =
  let reader, writer = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let t = { reader; writer; byte = Bytes.create 1 } in
  try
    Unix.set_nonblock writer;
    t
  with exn ->
    close t;
    raise exn

let rec signal t =
  try ignore (Unix.send_substring t.writer "x" 0 1 []) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> signal t
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()

let rec clear t =
  match Unix.recv t.reader t.byte 0 1 [] with
  | 0 -> failwith "causet: a wakeup socket closed while in use"
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> clear t

let wait t ~timeout =
  let timeout = if timeout < 0. then -1. else min 86_400. timeout in
  try
    if Sys.win32 then
      (* Winsock's select limits how many sockets are watched, not their
         handle values; here there is one. Avoid SO_RCVTIMEO: OCaml 5.3's
         Windows binding passes a timeval where Winsock expects DWORD ms. *)
      let readable, _, _ = Unix.select [ t.reader ] [] [] timeout in
      readable <> []
    else
      (* A zero receive timeout means no timeout. Round tiny finite waits
         up to one microsecond so conversion to timeval cannot disable it. *)
      let seconds = if timeout < 0. then 0. else max 0.000001 timeout in
      Unix.setsockopt_float t.reader Unix.SO_RCVTIMEO seconds;
      match Unix.recv t.reader t.byte 0 1 [ Unix.MSG_PEEK ] with
      | 0 -> failwith "causet: a wakeup socket closed while in use"
      | _ -> true
  with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> false
