(* This code is in the public domain *)


open Note
open Brr

(* This tests that nodes removed from the dom destroy their log.  If
   they didn't they would never be gc'd. Because logs hold a reference
   on count and vice-versa. *)

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
  let render_count count = [El.txtf "%d" count] in
  let count = S.hold [] (E.map render_count count) in
  El.def_children span count;
  span

let main () = match El.find ~id:(str "count") with
| None -> Log.err (fun m -> m "No element with id 'count' found.")
| Some c ->
    let count_value = S.hold [] (E.map (fun c -> [count_value count]) count) in
    El.def_children c count_value;
    run_count ()

let () = Brr.App.run main
