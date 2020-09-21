(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type level = Quiet | App | Error | Warning | Info | Debug
let _level = ref Debug
let level () = !_level
let set_level l = _level := l

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type 'a log = ('a, unit) msgf -> unit
type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

let pp_header ppf = function
| None -> ()
| Some v -> Format.fprintf ppf "[%s] " v

let console : level -> string -> unit =
  fun level s ->
  let meth = match level with
  | Error -> "error"
  | Warning -> "warn"
  | Info -> "info"
  | Debug -> "debug"
  | App -> "log"
  | Quiet -> assert false
  in
  ignore @@ Jv.call Brr.Console.(to_jv (get ())) meth [| Jv.of_string s |]


let report level k msgf =
  msgf @@ fun ?header fmt ->
  let k str = console level str; k () in
  Format.kasprintf k ("%a@[" ^^ fmt ^^ "@]@.") pp_header header

let nop_kmsg =
  let kmsg k level msgf = k () in
  { kmsg }

let default_kmsg =
  let kmsg k level msgf = match !_level with
  | Quiet -> k ()
  | level' when level > level' -> k ()
  | _ -> report level k msgf
  in
  { kmsg }

let _kmsg = ref default_kmsg
let set_kmsg kmsg = _kmsg := kmsg

let kunit _ = ()
let msg level msgf = !_kmsg.kmsg kunit level msgf
let app msgf = !_kmsg.kmsg kunit App msgf
let err msgf = !_kmsg.kmsg kunit Error msgf
let warn msgf = !_kmsg.kmsg kunit Warning msgf
let info msgf = !_kmsg.kmsg kunit Info msgf
let debug msgf = !_kmsg.kmsg kunit Debug msgf
let kmsg k level msgf = !_kmsg.kmsg k level msgf

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
