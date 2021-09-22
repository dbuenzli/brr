v0.0.2 2020-09-23 Zagreb
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
