(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Brr_canvas
open Fut.Syntax

let stripe_cnv_rect c ~x ~y ~w ~h =
  let x = truncate x and y = truncate y in
  let w = truncate w and h = truncate h in
  let idata = C2d.get_image_data c ~x ~y ~w ~h in
  let d = C2d.Image_data.data idata in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if x mod 4 <> 0 then () else
      let off = 4 * (y * w + x) in
      Tarray.set d (off    ) 0xFF;
      Tarray.set d (off + 1) 0x00;
      Tarray.set d (off + 2) 0x00
    done
  done;
  C2d.put_image_data c idata ~x ~y

let draw_brr c ~x ~y =
  let size = truncate (96. *. Window.device_pixel_ratio G.window) in
  C2d.set_font c Jstr.(v "bold " + of_int size + v "px SourceSansPro");
  C2d.fill_text c (Jstr.v " Brr!") ~x ~y

let draw_rect c ~x ~y ~w ~h =
  C2d.set_fill_style c (C2d.color (Jstr.v "#000"));
  C2d.fill_rect c ~x ~y ~w ~h

let draw cnv =
  let c = C2d.create cnv in
  let w = float @@ Canvas.w cnv in
  let h = float @@ Canvas.h cnv in
  C2d.stroke_rect c ~x:0. ~y:0. ~w ~h;
  let w = 0.5 *. w and h = 0.5 *. h in
  let x = w and y = h in
  draw_rect c ~x ~y ~w ~h;
  stripe_cnv_rect c ~x ~y ~w ~h;
  draw_brr c ~x:10. ~y:h

let set_size cnv =
  let el = Canvas.to_el cnv in
  let w = El.inner_w el in
  let h = Jstr.(of_int (truncate ((w *. 3.) /. 4.)) + v "px") (* 4:3 *) in
  El.set_inline_style El.Style.height h el;
  Canvas.set_size_to_layout_size cnv

let main ()  =
  let h1 = El.h1 [El.txt' "2D canvas"] in
  let info =
    let brr = El.strong [El.txt' "Brr!"] in
    [ El.txt' "Draws "; brr; El.txt' " and a black and red striped corner. " ]
  in
  let cnv = Canvas.create [] in
  let children = [h1; El.p info; Canvas.to_el cnv] in
  El.set_children (Document.body G.document) children;
  (* We need to wait for the stylesheet to access the font and setup the
     layout *)
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  set_size cnv; draw cnv; Fut.return ()

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
