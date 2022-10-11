(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Note

module Futr = struct
  let to_event f =
    let e, send_e = E.create () in
    let send_e v = ignore (G.set_timeout ~ms:0 @@ fun () -> send_e v) in
    Fut.await f send_e; e

  let of_event e =
    let fut, set_fut = Fut.create () in
    let logr = ref None in
    let set_fut v =
      (* N.B. this may be called immediately because of ~now:true.
         The delay ensure logr will be Some _ .*)
      ignore @@ G.set_timeout ~ms:0 @@ fun () ->
      match !logr with
      | None -> assert false
      | Some logr -> Logr.destroy logr; set_fut v
    in
    match E.log ~now:true e set_fut with
    | None -> None
    | Some _ as s -> logr := s; Some fut
end

module Consoler = struct
  let tick _ = Console.[Jstr.v "tick"]
  let log_value ?(l = Console.debug) ?(v = fun v -> Console.[v]) id x =
    l Console.(Jstr.(v id + v ":") :: (v x)); x

  module E = struct
    let log ?(obs = false) ?l ?v id e = match obs with
    | false -> E.map (log_value ?l ?v id) e
    | true ->
        Logr.may_hold (E.log e (fun ev -> ignore @@ log_value ?l ?v id ev)); e
  end

  module S = struct
    let log ?(obs = false) ?l ?v id s = match obs with
    | false -> S.map ~eq:(S.eq s) (log_value ?l ?v id) s
    | true -> Logr.hold (S.log s (fun sv -> ignore @@ log_value ?l ?v id sv)); s
  end
end

module Evr = struct
  let instruct ?(propagate = true) ?(default = true) e =
    (if default then () else Ev.prevent_default e);
    if propagate then () else Ev.stop_propagation e

  let endless_listen ?(capture = false) ?propagate ?default t type' f =
    let opts = match capture with
    | false -> None | true -> Some (Ev.listen_opts ~capture ())
    in
    let f ev = instruct ?propagate ?default ev; f ev in
    Ev.listen ?opts type' f t

  (* Note events *)

  let on_target ?(capture = false) ?propagate ?default type' f t =
    let opts = match capture with
    | false -> None | true -> Some (Ev.listen_opts ~capture ())
    in
    let e, send_e = E.create () in
    let f ev = instruct ?propagate ?default ev; send_e (f ev) in
    Ev.listen ?opts type' f t;
    e

  let on_targets ?(capture = false) ?propagate ?default type' f ts =
    let opts = match capture with
    | false -> None | true -> Some (Ev.listen_opts ~capture ())
    in
    let e, send_e = E.create () in
    let f ev = instruct ?propagate ?default ev; send_e (f (Ev.target ev) ev) in
    List.iter (Ev.listen ?opts type' f) ts;
    e

  let on_el ?capture ?propagate ?default type' f el =
    on_target ?capture ?propagate ?default type' f (El.as_target el)

  let on_els ?(capture = false) ?propagate ?default type' f els =
    let opts = match capture with
    | false -> None | true -> Some (Ev.listen_opts ~capture ())
    in
    let e, send_e = E.create () in
    let f ev =
      instruct ?propagate ?default ev;
      send_e (f (Obj.magic (* oh well *) (Ev.target ev) : El.t) ev)
    in
    List.iter (fun el -> Ev.listen ?opts type' f (El.as_target el)) els;
    e

  let unit e = ()
  let stamp v e = v
  let listen ?(capture = false) ?propagate ?default t type' f =
    let opts = match capture with
    | false -> None | true -> Some (Ev.listen_opts ~capture ())
    in
    let f ev = instruct ?propagate ?default ev; f ev in
    Ev.listen ?opts type' f t;
    fun () -> Ev.unlisten ?opts type' f t
end

module Elr = struct

  (* DOM garbage collection support. We observe HTML DOM additions and
     removals on body and invoke callback registered for the
     appropriate life cycle event. In particular Note loggers from
     nodes that are removed from the HTML DOM are destroyed. *)

  let xxx_funs xxx e : (unit -> unit) list = Obj.magic @@ Jv.get e xxx
  let add_xxx_fun xxx f e =
    let fs = Jv.get e xxx in
    let fs = if Jv.is_undefined fs then [f] else (f :: Obj.magic fs) in
    Jv.set e xxx (Jv.repr fs)

  let add_add_fun = add_xxx_fun "brr_add"
  let add_rem_fun = add_xxx_fun "brr_rem"
  let add_funs = xxx_funs "brr_add"
  let rem_funs = xxx_funs "brr_rem"

  let invoke_funs xxx node =
    let star = Jv.of_string "*" in
    let descendents n = Jv.call (El.to_jv n) "querySelectorAll" [| star |] in
    if not (El.is_el node) then () else
    let invoke_node_funs n =
      let funs = xxx_funs xxx n in
      List.iter (fun f -> f ()) funs;
      Jv.set n xxx (Jv.repr [])
    in
    let ns = descendents node in
    for i = 0 to (Jv.Int.get ns "length") - 1 do
      let n = Jv.call ns "item" [|Jv.of_int i|] in
      invoke_node_funs n
    done;
    invoke_node_funs (El.to_jv node)

  let () = (* Observe DOM additionals and removals *)
    let obs records _obs =
      let in_html_dom n =
        Jv.call (El.to_jv n) "getRootNode" [||] == Document.to_jv @@ G.document
      in
      for i = 0 to (Jv.Int.get records "length") - 1 do
        let r = Jv.Jarray.get records i in
        let adds = Jv.get r "addedNodes" in
        for i = 0 to (Jv.Int.get adds "length") - 1 do
          let n = El.of_jv @@ Jv.call adds "item" [|Jv.of_int i|] in
          if in_html_dom n then invoke_funs "brr_add" n
        done;
        let rems = Jv.get r "removedNodes" in
        for i = 0 to (Jv.Int.get rems "length") - 1 do
          let n = El.of_jv @@ Jv.call rems "item" [|Jv.of_int i|] in
          if not (in_html_dom n) then invoke_funs "brr_rem" n
        done
      done
    in
    let mutation_observer = Jv.get Jv.global "MutationObserver" in
    if Jv.is_none mutation_observer || Jv.is_none (Document.to_jv G.document)
    then ((* protect web worker *)) else
    let obs = Jv.new' mutation_observer [| Jv.repr obs |] in
    let opts = Jv.obj [| "childList", Jv.true'; "subtree", Jv.true' |] in
    let root = El.to_jv @@ Document.root G.document in
    ignore @@ Jv.call obs "observe" [| root; opts |]

  (* Logr gc *)

  let add_logr e l = add_rem_fun (fun () -> Logr.destroy l) (El.to_jv e)
  let may_add_logr e = function None -> () | Some l -> add_logr e l

  (* Children *)

  let set_children e ~on = may_add_logr e (E.log on (El.set_children e))
  let def_children e cs = add_logr e (S.log cs (El.set_children e))

  (* Attributes *)

  let set_at a ~on e = may_add_logr e (E.log on (fun v -> El.set_at a v e))
  let def_at a vs e = add_logr e (S.log vs (fun v -> El.set_at a v e))

  (* Classes *)

  let set_class c ~on e =
    may_add_logr e (E.log on (fun v -> El.set_class c v e))

  let def_class c bs e =
    add_logr e (S.log bs (fun v -> El.set_class c v e))

  (* Properties *)

  let set_prop p ~on e =
    may_add_logr e (E.log on (fun v -> El.set_prop p v e))

  let def_prop p vs e =
    add_logr e (S.log vs (fun v -> El.set_prop p v e))

  (* Style *)

  let set_inline_style ?important p ~on e =
    may_add_logr e (E.log on (fun v -> El.set_inline_style ?important p v e))

  let def_inline_style ?important p vs e =
    add_logr e (S.log vs (fun v -> El.set_inline_style ?important p v e))

  (* Focus *)

  let set_has_focus ~on e =
    may_add_logr e (E.log on (fun v -> El.set_has_focus v e))

  let def_has_focus b e =
    add_logr e (S.log b (fun v -> El.set_has_focus v e))

  (* Life-cycle callbacks *)

  let on_add f e = add_add_fun f (El.to_jv e)
  let on_rem f e = add_rem_fun f (El.to_jv e)

  (* Note loggers *)

  let call f ~on e = may_add_logr e (E.log on (fun v -> f v e))
  let hold_logr e l = add_logr e l
  let may_hold_logr e l = may_add_logr e l
end

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
