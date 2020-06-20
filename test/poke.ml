
(* Compile with:

   ocamlfind ocamlc -g -linkall -linkpkg -no-check-prims \
                       -package brr,brr.note,brr.poked poke.ml

   js_of_ocaml $(ocamlfind query -r -i-format brr.poked) -I . \
            --toplevel +toplevel.js +dynlink.js a.out -o poke.js *)

open Brr

let me () =
  El.set_children (Document.body G.document) El.[ h1 [ txt' "Revolt!" ]]

let main () =
  let h1 = El.h1 [El.txt' "OCaml console"] in
  let info = El.[
      p [txt' "This page has an OCaml poke object with the ";
         code [txt' "Brr"]; txt' ", "; code [txt' "Brr_note"]; txt' " and ";
         code [txt' "Brr_note_kit"]; txt' " libraries."];
      p [txt' "Install the OCaml console web extension, open the developer \
               tools, switch to the ‘OCaml’ tab and interact. Try:"];
      p [pre [txt' "Poke.me ();;"]];
      p [pre [txt' "open Brr;;"]];
      p [pre [txt' "Console.log [Document.body G.document];;"]]]
  in
  El.set_children (Document.body G.document) (h1 :: info)

let () = main ()
