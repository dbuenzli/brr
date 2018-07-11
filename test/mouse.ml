(* This code is in the public domain *)


open Note
open Brr

module M = Brr.Mouse

let pp_pt ppf (x, y) = Format.fprintf ppf "(%g %g)" x y
let pp_bool = Format.pp_print_bool

let main () =
  let h1 = El.(h1 [txt "Mouse test"]) in
  let div = El.div [] in
  let but = El.(button [txt "Destroy"]) in
  let but_click = Ev.(for_el but click unit) in
  let m = M.(for_el ~propagate:true div pt) in
  El.set_children (El.document_body ()) [h1; div; but];
  Logr.(may_hold @@ E.log but_click (fun () -> M.destroy m));
  ignore @@ Debug.trace_s ~obs:true pp_pt "pos" (M.pos m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "dpos" (M.dpos m);
  ignore @@ Debug.trace_s ~obs:true pp_bool "mem" (M.mem m);
  ignore @@ Debug.trace_s ~obs:true pp_bool "left" (M.left m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "left_up" (M.left_up m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "left_down" (M.left_down m);
  ignore @@ Debug.trace_s ~obs:true pp_bool "mid" (M.mid m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "mid_up" (M.mid_up m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "mid_down" (M.mid_down m);
  ignore @@ Debug.trace_s ~obs:true pp_bool "right" (M.right m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "right_up" (M.right_up m);
  ignore @@ Debug.trace_e ~obs:true pp_pt "right_down" (M.right_down m);
  ()

let () = Brr.App.run main
