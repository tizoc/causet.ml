(* Shared by the tests of this directory: what a failure says, as the Rust
   tests read a panic's message. *)

let contains ~sub s =
  let n = String.length s and k = String.length sub in
  let rec go i = i + k <= n && (String.sub s i k = sub || go (i + 1)) in
  go 0

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

(* The message an exception carries, as a Rust panic's payload. *)
let message_of = function
  | Causet_test.Failed { message; _ } -> message
  | Failure s | Invalid_argument s -> s
  | e -> Printexc.to_string e

(* Runs [f], which must raise; returns the message it raised with. *)
let fails f =
  match f () with
  | () -> Alcotest.fail "expected a failure, and it passed"
  | exception e -> message_of e

let ints = Alcotest.(list int)

let found_string (f : Causet_test.found) =
  Printf.sprintf "{ seed %d; key %S; choices [%s]; original %d; shrinks %d }" f.seed f.key
    (String.concat "; " (List.map string_of_int f.choices))
    f.original f.shrinks

let found_exn = function
  | Some f -> f
  | None -> Alcotest.fail "search found no failing run"
