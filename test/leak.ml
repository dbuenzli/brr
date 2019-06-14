(* This code is in the public domain *)

open Js_of_ocaml
open Note
open Brr

(* This tests that nodes removed from the HTML DOM destroy their log.  If
   they didn't they would never be garbage collected. *)

let count, run_count =
  let v = ref 0 in
  let count, send_count = E.create () in
  let rec run_count () =
    incr v; send_count !v;
    ignore (Dom_html.window ## (setTimeout (Js.wrap_callback run_count) (1.)))
  in
  count, run_count

let count_value count =
  (* Voluntarily silly. *)
  let p = El.p [] in
  let count_txt c = [`Txt (Jstr.vf "Steps: %d" c)] in
  let count = S.hold [] (E.map count_txt count) in
  El.def_children p count;
  p

let count_value_nest count =
  let p = El.p [] in
  let count_txt c = [`Txt (Jstr.vf "Steps (nest): %d" c)] in
  let count = S.hold [] (E.map count_txt count) in
  El.def_children p count;
  El.div [p]

let main () =
  let h1 = El.h1 [`Txt (Jstr.v "Leak test")] in
  let info = El.p [`Txt (Jstr.v
                           "Memory usage should be bounded and the step \
                            counter below should not slow down.")]
  in
  let step_count =
    let counts c = [count_value count; count_value_nest count] in
    S.hold [] (E.map counts count)
  in
  let steps = El.(p []) in
  let () = El.def_children steps step_count in
  El.set_children (El.document_body ()) [h1; info; steps];
  run_count ()

let () = Brr.App.run main
