(* The deriver alone, as a preprocessor: errors.t runs it on a file and
   reads its errors. *)
let () = Ppxlib.Driver.standalone ()
