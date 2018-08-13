(* This code is in the public domain *)


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
  let span = El.span [] in
  let count = S.hold [] (E.map count (fun c -> [`Txt (strf "Steps: %d" c)])) in
  El.def_children span count;
  span

let main () =
  let h1 = El.h1 [`Txt (str "Leak test")] in
  let info = El.p [`Txt (str "Memory usage should be bounded and the step \
                              counter below should not slow down.")]
  in
  let step_count = S.hold [] (E.map count (fun c -> [count_value count])) in
  let steps = El.(p []) in
  let () = El.def_children steps step_count in
  El.set_children (El.document_body ()) [h1; info; steps];
  run_count ()

let () = Brr.App.run main
