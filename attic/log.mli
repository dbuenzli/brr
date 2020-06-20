(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Console logging.

    The following functions log to the browser console. *)

type level = Quiet | App | Error | Warning | Info | Debug (** *)
(** The type for reporting levels. *)

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
(** The type for client specified message formatting functions. See
      {!Logs.msgf}. *)

type 'a log = ('a, unit) msgf -> unit
(** The type for log functions. See {!Logs.log}. *)

val msg : level -> 'a log
(** [msg l (fun m -> m fmt ...)] logs with level [l] a message
      formatted with [fmt]. *)

val app : 'a log
(** [app] is [msg App]. *)

val err : 'a log
(** [err] is [msg Error]. *)

val warn : 'a log
(** [warn] is [msg Warning]. *)

val info : 'a log
(** [info] is [msg Info]. *)

val debug : 'a log
(** [debug] is [msg Debug]. *)

val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
(** [kmsg k level m] logs [m] with level [level] and continues with [k]. *)

(** {1 Logging backend} *)

type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
(** The type for the basic logging function. The function is never
      invoked with a level of [Quiet]. *)

val set_kmsg : kmsg -> unit
(** [set_kmsg kmsg] sets the logging function to [kmsg]. *)

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
