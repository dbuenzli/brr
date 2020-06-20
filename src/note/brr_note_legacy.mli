(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Legacy functionality.

    Needed for transitional reasons. Will disappear when
    a suitable replacement is provided. Do not use. *)

open Note
open Brr

(** Persistent storage.

    Persisent key-value store implemented over
    {{:http://www.w3.org/TR/webstorage/}webstorage}. Safe if no one
    tampers with the storage outside of the program.

    {b XXX.}
    {ul
    {- This still relies on the jsoo representation, add safer keys with type
    indexed codecs.}
    {- Provide something sensitive for storage events}} *)
module Store : sig

  (** {1 Storage scope} *)

  type scope = [ `Session | `Persist ]
  (** The storage scope. *)

  (** {1 Keys} *)

  type 'a key
  (** The type for keys whose lookup value is 'a *)

  val key : ?ns:Jstr.t -> unit -> 'a key
  (** [key ~ns ()] is a new storage key in namespace [ns]. If [ns]
      is unspecified, the key lives in a global namespace.

      {b Warning.} Reordering invocations of {!key} in the same
      namespace will most of the time corrupt existing storage. This
      means that all {!key} calls should always be performed at
      initialization time. {!Store.force_version} can be used to
      easily version your store and aleviate this problem. *)

  (** {1 Storage}

      In the functions below [scope] defaults to [`Persist]. *)

  val mem : ?scope:scope -> 'a key -> bool
  (** [mem k] is [true] iff [k] has a mapping. *)

  val add : ?scope:scope -> 'a key -> 'a -> unit
  (** [add k v] maps [k] to [v]. *)

  val rem : ?scope:scope -> 'a key -> unit
  (** [rem k] unbinds [k]. *)

  val find : ?scope:scope -> 'a key -> 'a option
  (** [find k] is [k]'s mapping in [m], if any. *)

  val get : ?scope:scope -> ?absent:'a -> 'a key -> 'a
  (** [get k] is [k]'s mapping. If [absent] is provided and [m] has
      not binding for [k], [absent] is returned.

      @raise Invalid_argument if [k] is not bound and [absent]
      is unspecified or if [scope] is not {!support}ed. *)

  val clear : ?scope:scope -> unit -> unit
  (** [clear ()], clears all mapping. *)

  val ev : unit event
  (** [ev] fires on storage changes. FIXME provide something
      sensitive, e.g. key watching. *)

  (** {1 Versioning} *)

  val force_version : ?scope:scope -> string -> unit
  (** [force_version v] checks that the version of the store is [v].  If
      it's not it {!clear}s the store and sets the version to [v]. *)
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
