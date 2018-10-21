(* This code is in the public domain *)

open Note
open Brr

let main () =
  let hello = El.p [`Txt (Jstr.v "Hello horrible world !")] in
  El.set_children (El.document_body ()) [hello]

let () = App.run main
