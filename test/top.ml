(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Fut.Result_syntax
open Brr

let setup panel =
  let* ui = Brr_ocaml_poke_ui.create panel in
  Fut.bind (Brr_ocaml_poke.find ()) @@ function
  | Ok (Some poke) ->
      let drop_target = Document.as_target G.document in
      Brr_ocaml_poke_ui.run ui poke ~drop_target; Fut.ok ()
  | Ok None ->
      let msg = "No OCaml poke object found in page." in
      Brr_ocaml_poke_ui.output ~kind:`Warning ui [El.pre [El.txt (Jstr.v msg)]];
      Fut.ok ()
  | Error e ->
      Brr_ocaml_poke_ui.output ~kind:`Error ui [El.txt (Jv.Error.message e)];
      Fut.ok ()

let setup_theme () =
  let theme =
    if Window.prefers_dark_color_scheme G.window then "dark" else "light"
  in
  let html = Document.root G.document in
  El.set_at (Jstr.v "theme") (Some (Jstr.v theme)) html

let setup_body_style body =
  El.set_inline_style (Jstr.v "margin") (Jstr.v "0") body;
  El.set_inline_style (Jstr.v "padding") (Jstr.v "0") body;
  El.set_inline_style
    (Jstr.v "background-color") (Jstr.v "var(--ocaml-color-bg)") body;
  ()

let main () =
  Brr_poke.define ();
  let ui = El.div [] in
  let body = Document.body G.document in
  setup_body_style body;
  El.append_children (Document.body G.document) [ui];
  setup_theme ();
  setup ui

let () = ignore (main ())

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers

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
