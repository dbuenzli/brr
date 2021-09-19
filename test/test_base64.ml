(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Fut.Result_syntax

let text = Jstr.v "Program browsers in O\u{1F42B}."

let test log =
  log [El.txt Jstr.(v "encoding: " + text)];
  let data = Base64.data_utf_8_of_jstr text in
  let enc = Base64.encode data |> Result.get_ok in
  log [El.txt Jstr.(v "encoded : " + enc)];
  let dec = Base64.decode enc |> Result.get_ok in
  let text' = Base64.data_utf_8_to_jstr dec |> Result.get_ok in
  log [El.txt Jstr.(v "decoded : " + text')];
  if Jstr.equal text text'
  then log [El.txt Jstr.(v "Success.")]
  else log [El.txt Jstr.(v "ERROR: text did not round trip.")]

let main () =
  let h1 = El.h1 [El.txt' "Base64 test"] in
  let log_view = El.ol [] in
  let log cs = El.append_children log_view [El.li cs] in
  let children = [h1; log_view] in
  El.set_children (Document.body G.document) children;
  test log

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
