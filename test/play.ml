(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Note
open Brr

let setup () =
(*
  let tf = Ui.text_field (str "deactivate") in
  let label = Ui.text_field_value tf in
  let deactivate = Ui.button (S.map (fun v -> [`Txt v]) label) in
  let enabled = S.Bool.flip false (Ui.button_actuate deactivate) in
  let but = Ui.button ~enabled (S.const [El.txt "clack me"]) in
  let actuate () = [ Ui.text_field_el tf; ] in
  let actuate = E.map actuate (Ui.button_actuate but) in
  let init = [
    Ui.text_field_el tf; Ui.button_el deactivate; Ui.button_el but; ];
  in
  let body = S.hold ~eq:( == ) ~init actuate in
  El.rset_document_body body;
*)
  ()

let main () = setup (); ()

let () = App.run ~name:"play" main

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
