(* Mount identities match the Rust port's: xxh3-64 of every length up to
   300, covering each of the algorithm's paths, against the Rust crate. *)

let vectors () =
  let ic = open_in "xxh3_vectors.txt" in
  let rec go acc =
    match input_line ic with
    | line ->
        let i = String.rindex line ' ' in
        go
          ((String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))
          :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  go []

let every_length_matches_the_rust_port () =
  List.iter
    (fun (input, expected) ->
      let got = Printf.sprintf "%016Lx" (Causet.Xxh3.hash64 input) in
      Alcotest.(check string)
        (Printf.sprintf "length %d" (String.length input))
        expected got)
    (vectors ())

let () =
  Alcotest.run "xxh3"
    [
      ( "vectors",
        [
          Alcotest.test_case "every length matches" `Quick
            every_length_matches_the_rust_port;
        ] );
    ]
