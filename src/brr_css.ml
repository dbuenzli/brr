(*---------------------------------------------------------------------------
   Copyright (c) 2025 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr

(* CSS Custom Highlight API *)

module Highlight = struct
  type t = Jv.t
  include (Jv.Id : Jv.CONV with type t := t)

  let highlight = Jv.get Jv.global "Highlight"
  let create () = Jv.new' highlight [||]
  let priority h = Jv.Int.get h "priority"
  let size h = Jv.Int.get h "size"
  let type' h = Jv.Jstr.get h "type"
  let has h r = Jv.to_bool @@ Jv.call h "has" Jv.[| Range.to_jv r|]
  let add h r = ignore @@ Jv.call h "add" Jv.[| Range.to_jv r|]
  let delete h r = Jv.to_bool @@ Jv.call h "delete" Jv.[| Range.to_jv r|]
  let clear h = ignore @@ Jv.call h "clear" [||]
  let fold f h acc =
    let iter = Jv.call h "entries" Jv.[||] in
    Jv.It.fold Range.of_jv f iter acc
end

module Highlight_registry = struct
  type t = Jv.t
  include (Jv.Id : Jv.CONV with type t := t)

  let size r = Jv.Int.get r "size"
  let has r n = Jv.to_bool @@ Jv.call r "has" Jv.[| of_jstr n |]
  let get r n =
    Jv.to_option Highlight.of_jv @@ Jv.call r "get" Jv.[| of_jstr n |]

  let set r n h = ignore @@ Jv.call r "set" Jv.[|of_jstr n; Highlight.to_jv h |]
  let delete r n = Jv.to_bool @@ Jv.call r "delete" Jv.[| of_jstr n |]
  let clear r = ignore @@ Jv.call r "clear" [||]
  let fold f r acc =
    let iter = Jv.call r "entries" Jv.[||] in
    Jv.It.fold_bindings ~key:Jv.to_jstr ~value:Highlight.of_jv f iter acc
end

(* CSS object *)

module Css = struct
  let css = Jv.get Jv.global "CSS"
  let highlights () = Highlight_registry.of_jv @@ Jv.get css "highlights"
end
