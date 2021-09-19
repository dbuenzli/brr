

- Change the `Brr.Base64` module (`atob`, `bota`) to make it more
  useful and less error prone. Thanks to Armaël Guéneau for shooting
  himself in the foot (#18).
- Fix wrong value of `Request.Credentials.include`.
- Fix `Blob.of_array_buffer`, didn't work at all. Thanks to
  Armaël Guéneau for the report and the fix (#23).
- Add `Brr.Window.open'`. 
  Thanks to Boris Dob for the suggestion and the patch.
- Fix `Jstr.concat` when `sep` is unspecified (#14).
  Thanks to Sébastien Dailly for the report.
- Rename `Brr_webcrypto.crypto_algo.rsassa_pks1_v1_5` to `rsassa_pkcs1_v1_5`. 
  Thanks to Hannes Mehnert for the report and the fix.
- Fix signature of `Brr_webcrypto.Subtle_crypto.{export,import}_key`. 
  Thanks to Romain Calascibetta for the report and the fix.
- Add `Brr.El.parent` (#10).
  Thanks to Sébastien Dailly for the suggestion and the patch.
- Add `Brr.El.{find_first_by_selector,fold_find_by_selector}` to 
  lookup elements by CSS selectors.
- `Jstr.{starts_with,ends_with}`, change labels to follow Stdlib labelling. 
- Add optional `base` argument to `Brr.Uri.{v,of_jstr}`.
- Add `Brr.Uri.Params.is_empty`.
- Add `Brr_io.Form.Data.{is_empty,has_file_entry,of_uri_params,to_uri_params}`.

The following functions had their interface tweaked so as not to trigger
the 4.12 definition of the warning `unerasable-optional-argument`. 

* `Brr_canvas.Image_data.create`.
* `Brr_webaudio.Node.connect_node`
* `Brr_webaudio.Node.connect_param`


v0.0.1 2020-10-14 Zagreb
------------------------

First release. 
