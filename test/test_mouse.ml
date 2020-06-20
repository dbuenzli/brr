(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note
open Brr_note_kit

let show_pos (x, y) =
  let coord x = Jstr.of_float ~frac:4 x in
  [El.txt Jstr.(coord x + sp + coord y)]

let active id bool =
  let kbd = El.kbd (* semantic abuse *) [El.txt (Jstr.v id)] in
  Elr.def_class (Jstr.v "down") bool kbd;
  kbd

let pos pos =
  let view = El.code [] in
  Elr.def_children view (S.map show_pos pos); view

let ev_pos pos =
  let view = El.code [] in
  Elr.set_children view ~on:(E.map show_pos pos); view

let dir down up =
  let show_dir = El.span [] in
  let show d _ = [El.txt' (match d with `Up -> "↑" | `Down -> "↓")] in
  let dir = E.select [ E.map (show `Down) down; E.map (show `Up) up ] in
  Elr.set_children show_dir ~on:dir;
  show_dir

let button_viz evs name is_down down up =
  El.p [ active name (is_down evs); dir (down evs) (up evs); El.txt' "  ";
         ev_pos (E.select [down evs; up evs])]

let mouse_viz evs = El.div [
    El.p [ active "Inside" (Mouse.mem evs);
           pos (Mouse.pos evs); El.txt' "    Δ ";
           ev_pos (Mouse.dpos evs); ];
    button_viz evs "Left" Mouse.left Mouse.left_down Mouse.left_up;
    button_viz evs "Mid" Mouse.mid Mouse.mid_down Mouse.mid_up;
    button_viz evs "Right" Mouse.mid Mouse.right_down Mouse.right_up; ]

let area () =
  let area = El.div [] in
  El.set_inline_style El.Style.width (Jstr.v "75%") area;
  El.set_inline_style El.Style.height (Jstr.v "15rem") area;
  El.set_inline_style (Jstr.v "border") (Jstr.v "solid 1px black") area;
  area

let main () =
  let h1 = El.h1 [El.txt' "Mouse test"] in
  let info = El.txt' "Mouse and click in the area.  " in
  let area = area () in
  let m = Mouse.on_el ~propagate:true Mouse.pt area in
  let destroy = El.button [El.txt' "Destroy events"] in
  let destroy_click = Evr.on_el Ev.click Evr.unit destroy in
  let children = [h1; El.p [info; destroy];  area; mouse_viz m] in
  El.set_children (Document.body G.document) children;
  Logr.may_hold (E.log destroy_click (fun () -> Mouse.destroy m))

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
