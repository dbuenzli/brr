(* This code is in the public domain *)


open Note
open Brr

let pp_pt ppf (x, y) = Format.fprintf ppf "(%g %g)" x y
let pp_bool = Format.pp_print_bool
let txt s = `Txt (str s)

let main () =
  let module M = Brr.Mouse in
  let h1 = El.h1 [txt  "Mouse test"] in
  let p = El.p [txt "Mouse with the area and see the browser console."] in
  let div = El.div [] in
  let but = El.button [txt "Destroy events"] in
  let but_click = Ev.(for_el but click unit) in
  let m = M.(for_el ~propagate:true div pt) in
  let but_eff = E.log but_click (fun () -> M.destroy m) in
  El.set_children (El.document_body ()) [h1; p; div; El.p [but];];
  Logr.(may_hold but_eff);
  ignore @@ Debug.strace ~obs:true ~pp:pp_pt "pos" (M.pos m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "dpos" (M.dpos m);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "mem" (M.mem m);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "left" (M.left m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "left_up" (M.left_up m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "left_down" (M.left_down m);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "mid" (M.mid m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "mid_up" (M.mid_up m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "mid_down" (M.mid_down m);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "right" (M.right m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "right_up" (M.right_up m);
  ignore @@ Debug.etrace ~obs:true ~pp:pp_pt "right_down" (M.right_down m);
  ()

let () = Brr.App.run main
