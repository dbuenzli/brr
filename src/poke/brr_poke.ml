(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let js_to_string o =
  if Jv.is_null o then "null" else
  if Jv.is_undefined o then "undefined" else
  Jv.to_string (Jv.call o "toString" [||])

let pp_jv ppf v = Format.pp_print_string ppf (js_to_string v)
let pp_jstr ppf s = Format.fprintf ppf "@[Jstr.v %S@]" (Jstr.to_string s)
let pp_jv_error ppf (e : Jv.Error.t) =
  (* XXX add the stack trace *)
  Format.pp_print_string ppf (js_to_string (Jv.repr e))

let stdouts = ref "" (* for capturing toplevel outputs. *)
let stdouts_reset () = stdouts := ""
let stdouts_append d = stdouts := !stdouts ^ d
let resp = Buffer.create 100

let top_init () =
  (* FIXME investigate if this is a toplevel limitation or a
     js_of_ocaml one: we need to set the stdout/stderr flushers to
     capture errors and directive outputs. This means the poked program
     can't use them. *)
  Js_of_ocaml.Sys_js.set_channel_flusher stdout stdouts_append;
  Js_of_ocaml.Sys_js.set_channel_flusher stderr stdouts_append;
  Js_of_ocaml_toplevel.JsooTop.initialize ();
  (* FIXME we likely want to go differently about these things *)
  let ppf = Format.formatter_of_buffer resp in
  ignore (Js_of_ocaml_toplevel.JsooTop.use ppf
            "#install_printer Brr_poke.pp_jstr;;");
  ignore (Js_of_ocaml_toplevel.JsooTop.use ppf
            "#install_printer Brr_poke.pp_jv_error;;");
  ignore (Js_of_ocaml_toplevel.JsooTop.use ppf
            "#install_printer Brr_poke.pp_jv;;");
  ()

let top_eval phrase =
  let ppf = Format.formatter_of_buffer resp in
  stdouts_reset ();
  Js_of_ocaml_toplevel.JsooTop.execute true ppf (Jstr.to_string phrase);
  let r = Jstr.of_string (!stdouts ^ (Buffer.contents resp)) in
  Buffer.reset resp; stdouts_reset ();
  r

let top_use phrases =
  let ppf = Format.formatter_of_buffer resp in
  stdouts_reset ();
  let _bool = Js_of_ocaml_toplevel.JsooTop.use ppf (Jstr.to_string phrases) in
  let r = Jstr.of_string (!stdouts ^ (Buffer.contents resp)) in
  stdouts_reset (); Buffer.reset resp;
  r

let define () =
  let ocaml_version = Jstr.of_string Sys.ocaml_version in
  let jsoo_version = Jstr.of_string Js_of_ocaml.Sys_js.js_of_ocaml_version in
  let o =
    Jv.obj [| "version", Jv.of_int 0;
              "ocaml_version", Jv.of_jstr ocaml_version;
              "jsoo_version", Jv.of_jstr jsoo_version;
              "init", Jv.repr top_init;
              "eval", Jv.repr top_eval;
              "use", Jv.repr top_use; |]
  in
  Jv.set Jv.global "ocaml_poke" o

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
