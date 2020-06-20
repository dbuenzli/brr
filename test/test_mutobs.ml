(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

(* Testing the way we use mutation observers internally in Brr_note *)

let log op in_html_dom node =
  if not (El.is_el node) then Console.(log [str "Not an element"]) else
  let star = Jv.of_string "*" in
  let descendents n = Jv.call (El.to_jv n) "querySelectorAll" [| star |] in
  Console.(group [str "[%s] id:%s in_html_dom: %s";
                  (Jstr.v op); (El.prop El.Prop.id node); in_html_dom]);
  Console.(log [(descendents node)]);
  Console.group_end ();
  ()

let () =
  (* Moral equivalent of what we do in Brr_note.Elr.
     Observe DOM additionals and removals *)
  let obs records _obs =
    let in_html_dom n =
      Jv.call (El.to_jv n) "getRootNode" [||] == Document.to_jv @@ G.document
    in
    for i = 0 to (Jv.Int.get records "length") - 1 do
      let r = Jv.Jarray.get records i in
      let adds = Jv.get r "addedNodes" in
      for i = 0 to (Jv.Int.get adds "length") - 1 do
        let n = El.of_jv @@ Jv.call adds "item" [|Jv.of_int i|] in
        log "add" (in_html_dom n) n
      done;
      let rems = Jv.get r "removedNodes" in
      for i = 0 to (Jv.Int.get rems "length") - 1 do
        let n = El.of_jv @@ Jv.call rems "item" [|Jv.of_int i|] in
        log "rem" (in_html_dom n) n
      done
    done
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let obs = Jv.new' mutation_observer [| Jv.repr obs |] in
  let opts = Jv.obj [| "childList", Jv.true'; "subtree", Jv.true' |] in
  let root = El.to_jv (Document.root G.document) in
  ignore @@ Jv.call obs "observe" [| root; opts |]

let el_id eid cs = El.div ~at:At.[id (Jstr.v eid)] cs
let d0 = el_id "d0" [el_id "i0" [el_id "i00" []]; el_id "i1" [];]
let d1 = el_id "d1" []

let main () =
  let async f = ignore (G.set_timeout f ~ms:0) in
  let body = Document.body G.document in
  async (fun () -> El.set_children body [d0]);
  async (fun () -> El.set_children body [d1]);
  async (fun () -> El.set_children d1 [el_id "i2" []]);
  async (fun () -> El.set_children body [d0]);
  async (fun () -> El.set_children body [d1];);
  ()

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
