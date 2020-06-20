(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [ocaml_poke] object for OCaml console.

    See the {{!page-ocaml_console}OCaml console manual}
    for more information. *)

(** {1:poke Poke objects} *)

type t
(** The type for OCaml poke objects. Note that the actual object
    may live in another global context. *)

val version : t -> int
(** [version p] is the version of the poke object interface. *)

val ocaml_version : t -> Jstr.t
(** [ocaml_version p] is the OCaml version being poked by [p]. *)

val jsoo_version : t -> Jstr.t
(** [jsoo_version p] is the [js_of_ocaml] version being poked by [p]. *)

val eval : t -> Jstr.t -> Brr.Json.t Fut.or_error
(** [eval expr] evaluates the given OCaml toplevel phrase in the poke
    object and returns the result as a JSON string. *)

val use : t -> Jstr.t -> Brr.Json.t Fut.or_error
(** [use phrases] silently evaluates the given OCaml toplevel phrases in
    the poke object and returns possible errors via a JSON string. *)

(** {1:finding Finding poke objects} *)

val find : unit -> t option Fut.or_error
(** [find ()] looks for and initalizes an OCaml poke object in the global
    context of the caller. *)

val find_eval'd :
  eval:(Jstr.t -> Brr.Json.t Fut.or_error) -> t option Fut.or_error
(** [find_eval'd] looks for and initializes an OCaml poke object by using
    the given JavaScript [eval] function. *)

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
