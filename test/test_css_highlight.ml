(*---------------------------------------------------------------------------
   Copyright (c) 2025 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Brr_css

let text = (* https://doi.org/10.1145/365230.365257 *)
  "The system is biased towards “expressions” rather than “statements.” \
   It includes a nonprocedural (purely functional) subsystem that aims \
   to expand the class of users' needs that can be met by a single \
   print-instruction, without sacrificing the important properties \
   that make conventional right-hand-side expressions easy to construct \
   and understand."

let mark marker text search _ev =
  let keyword = El.prop El.Prop.value search in
  let content = El.text_content text in
  let () = Highlight.clear marker in
  if Jstr.is_empty keyword then () else
  let rec loop marker ~start text keyword =
    match Jstr.find_sub ~start ~sub:keyword content with
    | None -> ()
    | Some i ->
        let next = i + Jstr.length keyword in
        let r = Range.create () in
        Range.set_start r text i; Range.set_end r text next;
        Highlight.add marker r;
        loop marker ~start:(next + Jstr.length keyword) text keyword
  in
  loop marker ~start:0 text keyword

let main () =
  (* Document *)
  let h1 = El.h1 [El.txt' "CSS Custom Highlight"] in
  let search = El.input ~at:At.[type' (Jstr.v "search")] () in
  let ui = El.p [El.label [El.txt' "Highlight: "; search]] in
  let text = El.txt' text in
  El.set_children (Document.body G.document) [h1; ui; El.p [text]];
  (* Highlighter setup *)
  let css = Jstr.v "marker" (* ::highlight(marker) defined in base.css *) in
  let marker = Highlight.create () in
  let on_input = mark marker text search in
  let () = Highlight_registry.set (Css.highlights ()) css marker in
  let () = ignore (Ev.listen Ev.input on_input (El.as_target search)) in
  ()

let () = main ()
