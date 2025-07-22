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

let mark show_error marker text search _ev =
  let () = Highlight.clear marker in
  let () = show_error Jstr.empty in
  let regexp = El.prop El.Prop.value search in
  let content = El.text_content text in
  if Jstr.is_empty regexp then () else
  match Regexp.create ~flags:(Jstr.v "g") regexp with
  | exception Jv.Error e -> show_error (Jv.Error.message e)
  | regexp ->
      let highlight result () =
        let r = Range.create () in
        Range.set_start r text (Regexp.Result.start_index result);
        Range.set_end r text (Regexp.Result.stop_index result);
        Highlight.add marker r;
      in
      Regexp.fold_matches regexp highlight content ()

let main () =
  (* Document *)
  let h1 = El.h1 [El.txt' "CSS Custom Highlight"] in
  let search = El.input ~at:At.[type' (Jstr.v "search")] () in
  let err = El.code [El.txt' ""] in
  let show_error msg = El.set_children err [El.txt msg] in
  let ui =
    El.p [El.p [El.label [El.txt' "Highlight (regexp): "; search]];
          El.pre [err; El.txt' " " (* Make sure there's a line *) ]]
  in
  let text = El.txt' text in
  El.set_children (Document.body G.document) [h1; ui; El.p [text]];
  (* Highlighter setup *)
  let css = Jstr.v "marker" (* ::highlight(marker) defined in base.css *) in
  let marker = Highlight.create () in
  let on_input = mark show_error marker text search in
  let () = Highlight_registry.set (Css.highlights ()) css marker in
  let () = ignore (Ev.listen Ev.input on_input (El.as_target search)) in
  ()

let () = main ()
