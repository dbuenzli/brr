(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml poke object definition for the OCaml console.

    See the {{!page-ocaml_console}OCaml console documentation}
    for more information. *)

val define : unit -> unit
(** [define ()] defines a global
    {{!page-ocaml_console.ocaml_poke}[ocaml_poke]} object in
    the global context of the caller.

    {b Limitation.} Due to {!Js_of_ocaml_toplevel.JsooTop}, this poke
    object sets channel flusher via
    {!Js_of_ocaml.Sys.set_channel_flusher} for [stdout] and [stderr].
    This will not work if your application makes use of these
    channels. It's unclear whether this limitation can be easily
    lifted. *)

val pp_jstr : Format.formatter -> Jstr.t -> unit
val pp_jv_error : Format.formatter -> Jv.Error.t -> unit
val pp_jv : Format.formatter -> Jv.t -> unit

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
