(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note

(* This tests that nodes removed from the HTML DOM destroy their log.  If
   they didn't they would never be garbage collected. *)

let count, run_count =
  let v = ref 0 in
  let count, send_count = E.create () in
  let rec run_count () =
    incr v; send_count !v; ignore (G.set_timeout ~ms:0 run_count)
  in
  count, run_count

let count_value count =
  (* Voluntarily silly. *)
  let p = El.p [] in
  let count_txt c = [El.txt Jstr.(v "Steps: " + of_int c)] in
  let count = S.hold [] (E.map count_txt count) in
  Elr.def_children p count;
  p

let count_value_nest count =
  let p = El.p [] in
  let count_txt c = [El.txt Jstr.(v "Steps (nest): " + of_int c)] in
  let count = S.hold [] (E.map count_txt count) in
  Elr.def_children p count;
  El.div [p]

let steps () =
  let steps = El.div [] in
  let children =
    let counts c = [count_value count; count_value_nest count] in
    S.hold [] (E.map counts count)
  in
  Elr.def_children steps children;
  steps

let main () =
  let h1 = El.h1 [El.txt' "No leaks!"] in
  let i = "Memory usage must be bounded and the counters must not slow down." in
  let info = El.p [El.txt' i ] in
  El.set_children (Document.body G.document) [h1; info; steps ()];
  run_count ()

let () = main ()

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
