(*---------------------------------------------------------------------------
   Copyright (c) 2025 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CSS related APIs. *)

open Brr

(** {1:css CSS Custom Highlight API}

    See the API documentation on
    {{:https://developer.mozilla.org/en-US/docs/Web/API/CSS_Custom_Highlight_API}MDN}. *)

(** [Highlight] objects. *)
module Highlight : sig
  type t
  (** The type for
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight}
      [Highlight]} objects. *)

  val create : unit -> t
  (** [create ()] is a new highlight object. *)

  val priority : t -> int
  (** [priority h] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/priority}
      priority} of [h]. *)

  val size : t -> int
  (** [size h] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/size}size}
      of [h]. *)

  val type' : t -> Jstr.t
  (** [type t] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/type}type}
      of [h]. *)

  val has : t -> Range.t -> bool
  (** [has h r] is [true] if [h] {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/has}has} the range [r]. *)

  val add : t -> Range.t -> unit
  (** [add h r] {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/add}adds} adds the range [r] to [h]. *)

  val clear : t -> unit
  (** [clear h] {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/clear}removes} all range. *)

  val delete : t -> Range.t -> bool
(** [delete h r] {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/delete}deletes} the range [r] and returns [true] if there
    was such an range and [false] otherwise. *)

  val fold : (Range.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  (** [fold f h acc] folds [f] over the {{:https://developer.mozilla.org/en-US/docs/Web/API/Highlight/entries}ranges} of [h] starting with
      [acc]. *)

  (**/**)
  include Jv.CONV with type t := t
  (**/**)
end

(** [HighlightRegistry] objects. *)
module Highlight_registry : sig
  type t
  (** The type for {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry}[HighlightRegistry]} objects. *)

  val size : t -> int
  (** [size r] is the {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/size}size} of [r]. *)

  val has : t -> Jstr.t -> bool
  (** [has r n] is [true] if [r] {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/has}has} a highlight for the name [n]. *)

  val get : t -> Jstr.t -> Highlight.t option
  (** [get r n] {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/get}gets} the highlight registered with [n], if any. *)

  val set : t -> Jstr.t -> Highlight.t -> unit
  (** [set r n h] {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/set}sets} the entry named [n] to highlight [h]. *)

  val delete : t -> Jstr.t -> bool
(** [delete r n] {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/delete}deletes} the entry named by [n] and returns [true] if there
    was such an entry and [false] otherwise. *)

  val clear : t -> unit
  (** [clear r] {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/clear}removes} all entries. *)

  val fold : (Jstr.t -> Highlight.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  (** [fold f r acc] folds [f] over the {{:https://developer.mozilla.org/en-US/docs/Web/API/HighlightRegistry/entries}entries} of [r] starting with
      [acc]. *)

  (**/**)
  include Jv.CONV with type t := t
  (**/**)
end

(** {1:css CSS object} *)

(** The [CSS] object.

    This module represents the
    {{:https://developer.mozilla.org/en-US/docs/Web/API/CSS}CSS} object. *)
module Css : sig
  val highlights : unit -> Highlight_registry.t
  (** [highlights ()] is the static
      {{:https://developer.mozilla.org/en-US/docs/Web/API/CSS/highlights_static}
      highlights registry}. *)
end
