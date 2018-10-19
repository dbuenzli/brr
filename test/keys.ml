(* This code is in the public domain *)

open Note
open Brr

let log_keys_to e =
  let key = Ev.(for_el (El.document_body ()) keydown Key.of_ev) in
  let key_txt k = [`Txt (Jstring.vf "Last key down: %a" Key.pp k)] in
  let log = E.map key_txt key in
  El.rset_children e ~on:log

let pp_bool = Format.pp_print_bool

let main () =
  let h1 = El.h1 [`Txt (Jstring.v "Keyboard test")] in
  let info = Jstring.v "Hit the keyboard and see the browser console." in
  let info = El.p [`Txt info] in
  El.set_children (El.document_body ()) [h1; info];
  let k = Key.(for_el (El.document_body ())) in
  ignore @@ Debug.etrace ~obs:true ~pp:Key.pp "any_down" (Key.any_down k);
  ignore @@ Debug.etrace ~obs:true ~pp:Key.pp "any_up" (Key.any_up k);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "any_holds" (Key.any_holds k);
  ignore @@ Debug.etrace ~obs:true ~pp:Debug.tick "down (spacebar)"
    (Key.down k `Spacebar);
  ignore @@ Debug.etrace ~obs:true ~pp:Debug.tick "up (spacebar) "
    (Key.up k `Spacebar);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "holds (spacebar)"
    (Key.holds k `Spacebar);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "alt" (Key.alt k);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "ctrl" (Key.ctrl k);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "meta" (Key.meta k);
  ignore @@ Debug.strace ~obs:true ~pp:pp_bool "shift" (Key.shift k);
  ()

let () = Brr.App.run main
