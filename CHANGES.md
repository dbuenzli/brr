
- Add `Brr.File.relative_path`.
- Add `Brr.At.float`.
- Add `Brr.At.{void,is_void,if',if_some}` and deprecate
  `Brr.At.{add_if,add_if_some}`. The new scheme if more convenient 
  and clearer when working with list literals.
- Fix `Brr_canvas.C2d.transform` binding to `resetTransform` instead
  of `transform` (#38).
- Add `Brr_canvas.C2d.{set_transform',transform'}` taking matrix
  components directly.
- Add `Jstr.binary_{of,to}_octets` to convert between OCaml strings
  as sequence of bytes and JavaScript binary strings (#18 again)
- Add `Brr_webmidi`, bindings for Web MIDI.
- Make the modules' initialisation bits web worker safe.  We have
  toplevel code that access properties of values that (e.g. `Brr.El`
  accessing `document` or `Brr_note` accessing mutation
  observers). These modules can't be used in web workers but they may
  be linked in your web worker code (e.g. if you fork()-like your
  workers) in which case toplevel initialisation bits do get executed.

v0.0.3 2022-01-30 La Forclaz (VS)
---------------------------------

- Require `js_of_ocaml` 4.0.0:

  * Allows `brr`, `js_of_ocaml`, and `gen_js_api` bindings to be used in the 
    same program.
  * Adding `-no-check-prims` during bytecode linking is no longer required.

  Thanks to Hugo Heuzard for making the ground work in `js_of_ocaml` and 
  providing a patch (#2, #33).
  
- Add `Brr.Ev.beforeunload`.
- Add `Brr.Ev.Pointer.as_mouse`.
- Tweak `Brr.Ev.{Drag,Wheel}.as_mouse_event` into 
  `Brr.Ev.{Drag,Wheel}.as_mouse` to avoid coercion madness.
- Add `Brr.El.{previous,next}_sibling`.
- Add `Brr.El.remove_inline_style`.
- Add `Brr.El.Style.{top,left,right,bottom,position,z_index}`.
- Fix `Blob.of_jstr`. It was not working. Thanks to Kiran Gopinathan for
  the report (#31).
- `Ev.target_{of,to}_jv` take and return a `Jv.t` value instead of an `'a`.
  Thanks to Joseph Price for the report (#28).

v0.0.2 2021-09-23 Zagreb
------------------------

- Change the `Brr.Base64` module (`atob`, `bota`) to make it more
  useful and less error prone (#18). 
  Thanks to Armaël Guéneau for shooting himself in the foot.
- Add `Brr.Window.open'` (#20). 
  Thanks to Boris Dob for the suggestion and the patch.
- Rename `Brr_webcrypto.Crypto_algo.rsassa_pks1_v1_5` to `rsassa_pkcs1_v1_5`. 
  Thanks to Hannes Mehnert for the report and the fix.
- Add `Brr.El.parent` (#10).
  Thanks to Sébastien Dailly for the suggestion and the patch.
- Add `Brr.El.{find_first_by_selector,fold_find_by_selector}` to 
  lookup elements by CSS selectors.
- `Jstr.{starts_with,ends_with}`, change labels to follow Stdlib labelling. 
- Add optional `base` argument to `Brr.Uri.{v,of_jstr}`.
- Add `Brr.Uri.Params.is_empty`.
- Add `Brr_io.Form.Data.{is_empty,has_file_entry,of_uri_params,to_uri_params}`.
- Tweak interfaces of `Brr_canvas.Image_data.create`, 
  `Brr_webaudio.Node.connect_node`, `Brr_webaudio.Node.connect_param` to
  not trigger the 4.12 definition of the warning 
  `unerasable-optional-argument`. 
- Fix for uncaught JavaScript exceptions in the OCaml console (#21). 
  The fix is brittle, the right fix is to solve (#2).
- Fix `Brr_canvas.Gl` for browsers that do not support GL2 contexts.
  On these browsers this would lead to obscure failures in separate
  compilation mode. 
  Thanks to Duncan Holm for the report (#9).
- Fix wrong value of `Request.Credentials.include`.
- Fix `Blob.of_array_buffer` (#23). Didn't work at all. 
  Thanks to Armaël Guéneau for the report and the fix.
- Fix `Jstr.concat` when `sep` is unspecified (#14).
  Thanks to Sébastien Dailly for the report.
- Fix signature of `Brr_webcrypto.Subtle_crypto.{export,import}_key`. 
  Thanks to Romain Calascibetta for the report and the fix.

v0.0.1 2020-10-14 Zagreb
------------------------

First release. 
