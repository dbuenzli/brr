(* This code is in the public domain *)

open Js_of_ocaml
open Note
open Brr

(* Testing the way we use mutation observers internally. *)

let star = Js.Unsafe.inject @@ Jstr.v "*"
let descendents n =
    ((Js.Unsafe.meth_call n "querySelectorAll" [|star|]) :>
       Dom.element Js.t Dom.nodeList Js.t)

let in_html_dom n =
  (Js.Unsafe.meth_call n "getRootNode" [||] :> #Dom.node Js.t) ==
  Dom_html.document

let log_obj op n =
  Debug.pr "[%s] id:%a in_html_dom %b"
    op Jstr.pp Prop.(get id n) (in_html_dom n);
  Debug.dump_obj n;
  begin match (n ##. nodeType) with
  | Dom.ELEMENT -> Debug.dump_obj (descendents n);
  | _ -> ()
  end

let obs = match MutationObserver.is_supported () with
| false ->
    Log.warn (fun m -> m "No MutationObserver, disfunction ahead!"); Js.null
| true ->
    let f records observer =
      for i = 0 to records ##. length - 1 do
        match Js.Optdef.to_option (Js.array_get records i) with
        | None -> ()
        | Some r ->
            let adds = r ##. addedNodes in
            for i = 0 to adds ##. length - 1 do
              match Js.Opt.to_option (adds ## item i) with
              | None -> ()
              | Some o -> log_obj "add" o
            done;
            let rems = r ##. removedNodes in
            for i = 0 to rems ##. length - 1 do
              match Js.Opt.to_option (rems ## item i) with
              | None -> ()
              | Some o -> log_obj "rem" o
            done
      done
    in
    Js.some @@ MutationObserver.observe
      ~node:(Dom_html.document ##. documentElement :> Dom.node Js.t) ~f
      ~child_list:true ~subtree:true ~attributes:false ~character_data:false
      ~attribute_old_value:false ~character_data_old_value:false ()

let el_id eid cs = El.div ~atts:Att.[id (Jstr.v eid)] cs

let d0 = el_id "d0" [el_id "i0" [el_id "i00" []]; el_id "i1" [];]
let d1 = el_id "d1" []

let main () =
  let async f = Time.delay 0. @@ f in
  async (fun () -> El.set_children (El.document_body ()) [d0]);
  async (fun () -> El.set_children (El.document_body ()) [d1]);
  async (fun () -> El.set_children d1 [el_id "i2" []]);
  async (fun () -> El.set_children (El.document_body ()) [d0]);
  async (fun () -> El.set_children (El.document_body ()) [d1];);
  ()


let () = Brr.App.run main
