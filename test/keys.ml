(* This code is in the public domain *)


open Note
open Brr

let log_keys span =
  let key = Key.(el_event down (El.document_body ())) in
  let log = E.map (fun k -> [El.txtf "%a" Key.pp k]) key in
  El.rset_children span ~on:log

let main () =
  let id = str "app" in
  match El.find ~id with
  | None -> Log.err (fun m -> m "No element with id '%a' found." Str.pp id)
  | Some span -> log_keys span


let () = Brr.App.run main
