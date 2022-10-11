(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_io
open Brr_note

(* Unsafe encoding of OCaml values according to

   https://github.com/ocsigen/js_of_ocaml/blob/master/lib/js_of_ocaml/json.ml

   Get rid of this. *)

let json = Jv.get Jv.global "JSON"

external string_of_jsbytes : Jv.t -> Jv.t = "caml_string_of_jsbytes"
external string_to_jsbytes : Jv.t -> Jv.t = "caml_jsbytes_of_string"
external int64_lo_mi_hi : int -> int -> int -> Jv.t
  = "caml_int64_create_lo_mi_hi"

let int64_to_jv v =
  Jv.of_jv_array Jv.[| of_int 255; get v "lo"; get v "mi"; get v "hi" |]

let int64_of_jv v =
      int64_lo_mi_hi
        (Jv.to_int (Jv.Jarray.get v 1)) (Jv.to_int (Jv.Jarray.get v 2))
        (Jv.to_int (Jv.Jarray.get v 3))

let encode_ocaml_value v =
  let string = Jv.get (Jv.repr "") "constructor" in
  let int64 = Jv.get (Jv.repr 1L) "constructor" in
  let replacer _key v =
    if Jv.instanceof v ~cons:string then string_to_jsbytes v else
    if Jv.instanceof v ~cons:int64 then int64_to_jv v else
    v
  in
  Jv.to_jstr (Jv.call json "stringify" Jv.[| repr v; repr replacer |])

let decode_unsafe_ocaml_value s =
  let jsarray = Jv.get (Jv.repr (Jv.Jarray.create 0)) "constructor" in
  let reviver _key v =
    (* XXX this will also revive Jstr.t values as OCaml strings.
       replacer should tag ocaml strings, it does not for now.
       We should get rid of this anyways. *)
    if Jstr.equal (Jv.typeof v) (Jstr.v "string")
    then string_of_jsbytes v else
    if Jv.instanceof v ~cons:jsarray && Jv.Jarray.length v == 4 &&
       Jv.to_int (Jv.Jarray.get v 0) = 255
    then (int64_of_jv v)
    else v
  in
  Obj.magic (Jv.call json "parse" Jv.[| of_jstr s; repr reviver |])

module Store = struct

  type scope = [ `Session | `Persist ]

  let scope_store = function
  | `Session -> Storage.session G.window
  | `Persist -> Storage.local G.window

  type 'a key = Jstr.t

  let key_prefix = Jstr.v "k"
  let key =
    let id = ref (-1) in
    fun ?ns () ->
      id := !id + 1;
      let id = Jstr.of_int !id in
      match ns with
      | None -> Jstr.(key_prefix + id)
      | Some ns -> Jstr.(ns + v "-" + key_prefix + id)

  let version = key ~ns:(Jstr.v "brr") ()

  let mem ?(scope = `Persist) k =
    Storage.get_item (scope_store scope) k <> None

  let add ?(scope = `Persist) k v =
    (Storage.set_item (scope_store scope) k (encode_ocaml_value v))
    |> Console.log_if_error ~use:()

  let rem ?(scope = `Persist) k = Storage.remove_item (scope_store scope) k
  let find ?(scope = `Persist) k =
    match Storage.get_item (scope_store scope) k with
    | None -> None
    | Some v -> Some (decode_unsafe_ocaml_value v)

  let get ?(scope = `Persist) ?absent k =
    let absent () = match absent with
    | None -> invalid_arg "key unbound"
    | Some v -> v
    in
    match Storage.get_item (scope_store scope) k with
    | None -> absent ()
    | Some v -> decode_unsafe_ocaml_value v

  let clear ?(scope = `Persist) () = Storage.clear (scope_store scope)
  let force_version ?(scope = `Persist) v =
    match find ~scope version with
    | None -> add ~scope version v
    | Some sv ->
        if v <> sv then (clear ~scope (); add ~scope version v)

  let storage = Ev.Type.void (Jstr.v "storage")
  let ev =
    (* protect web workers *)
    if Jv.is_none (Window.to_jv G.window) then E.never else
    (Evr.on_target storage (fun _ -> ()) (Window.as_target G.window))
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
