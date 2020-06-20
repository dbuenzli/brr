(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Brr_io
open Brr_webworkers
open Fut.Result_syntax

let handle_error ~view = function
| Ok () ->  ()
| Error e ->
    let err = Jv.Error.message e in
    let msg = El.p [El.txt Jstr.(v "An error occured: " + err)] in
    El.set_children view [msg]

let notif_options () =
  let j = Jstr.v in
  let hot = Notification.Action.v ~action:(j "hot") ~title:(j "Hot") () in
  let cold = Notification.Action.v ~action:(j "cold") ~title:(j "Cold") () in
  Notification.opts ~actions:[hot; cold] ()

let show_notification () =
  let c = Service_worker.Container.of_navigator G.navigator in
  let* r = Service_worker.Container.register c (Jstr.v "test_notification.js")in
  let t = Jstr.v "Brr!" and opts = notif_options () in
  let* () = Service_worker.Registration.show_notification r t ~opts in
  Fut.ok ()

let notify_me view () =
  ignore @@ Fut.map (handle_error ~view) @@
  let* perm = Notification.request_permission () in
  El.set_children view [El.p [El.txt Jstr.(v "Permission: " + perm)]];
  if Jstr.equal perm Notification.Permission.granted
  then show_notification ()
  else Fut.ok ()

let button ?at onclick label =
  let but = El.button ?at [El.txt (Jstr.v label)] in
  Ev.listen Ev.click (fun _e -> onclick ()) (El.as_target but); but

let page_main () =
  let h1 = El.h1 [El.txt' "Notification test"] in
  let info = El.p [ El.strong [El.txt' "Note."];
                    El.txt' " Doesn't work over the file:// protocol."]
  in
  let view = El.p [] in
  let notify_me = button (notify_me view) "Notify me" in
  let children = [h1; info; El.p [notify_me]; view] in
  El.set_children (Document.body G.document) children

let () = if Worker.ami () then () else page_main ()

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
