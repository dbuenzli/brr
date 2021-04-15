(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Infrastructure for reactive browser interaction.

    This is the low-level infrastructure to enable reactive
    browser programming. See {!Brr_note_kit} for more fancy stuff. *)

open Note
open Brr

(** Futures as {!Note} events and vice-versa. *)
module Futr : sig

  val to_event : 'a Fut.t -> 'a event
  (** [to_event fut] is an event that occurs {e once} an infinitesimal amount
      of time after [fut] does. This means at earliest at the next
      JavaScript event loop cycle, even if [fut] is already determined. *)

  val of_event : 'a event -> 'a Fut.t option
  (** [of_event e] is a future that determines an infinitesimal amount
      of time after the current or next occurence of [e]. This means
      at earliest at the next JavaScript event loop cycle, even if [e]
      occurs now. [e] is observed only once by the future. [None] is returned
      if [e] never occurs. *)
end

(** Support for logging Note events and signals.  *)
module Consoler : sig

  val tick : 'a Console.msgr
  (** [tick] formats ["tick"] on any value. *)

  val log_value : ?l:Console.log -> ?v:'a Console.msgr -> string -> 'a -> 'a
  (** [log_value ~l ~v id x] is [x] but logs [x] with [l] (defaults to
      {!Brr.Console.debug}) formatted by [v] (defaults
      {!Brr.Console.msg}) and prepended by [id]. *)

  (** Logging events. *)
  module E : sig
    val log :
      ?obs:bool -> ?l:Console.log -> ?v:'a Console.msgr -> string ->
      'a event -> 'a event
    (** [event] is like {!log_value} on [e]'s occurences. If [obs] is
        [true] the returned value is [e] itself and the tracing occurs
        through a logger; note that this prevents [e] from being garbage
        collected. If [obs] is false, the return value is [e] mapped
        by a side effecting identity. *)
  end

  (** Logging signal changes. *)
  module S : sig
    val log :
      ?obs:bool -> ?l:Console.log -> ?v:'a Console.msgr -> string ->
      'a signal -> 'a signal
    (** [signal] is like {!log_value} but on the changes of [s]. If
        [obs] is [true], the return value is [s] itself and the
        tracing occurs through a logger, note that this prevents [s]
        from being garbage collected. If [obs] is [false], the return
        value is [s] mapped by a tracing identity and using [s]'s
        equality function. *)
  end
end

(** DOM events as {!Note} events. *)
module Evr : sig

  val on_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'b Ev.type' ->
    ('b Ev.t -> 'c) -> Ev.target -> 'c event
  (** [on_target ~capture ~propagate ~default et f t] is an event that
      reports events of type [et] transformed by [f] on target [t] such
      that:
      {ul
      {- If [capture] is [true] the event occurs during the capture phase.
         Defaults to [false].}
      {- If [propagate] is [true] the event is propagated. Defaults to [true].}
      {- If [default] is [true] the default behaviour is performed. Defaults
         to [true].}} *)

  val on_targets :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'b Ev.type' ->
    (Ev.target -> 'b Ev.t -> 'c) -> Ev.target list -> 'c event
  (** {!on_targets} is like {!for_target} except the event occurs
      for the event kind on the given list of targets. *)

  val on_el :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'b Ev.type' ->
    ('b Ev.t -> 'c) -> El.t -> 'c event
  (** [on_el et f el] is [for_target et f (El.as_target el)]. *)

  val on_els :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'b Ev.type' ->
    (El.t -> 'b Ev.t -> 'c) -> El.t list -> 'c event
  (** [on_els et f els] is [for_targets et f (List.map El.as_target els)]. *)

  (** {1:evmap Event mappers} *)

  val unit : 'b Ev.t -> unit
  (** [unit e] is [()]. *)

  val stamp : 'a -> 'b Ev.t -> 'a
  (** [stamp v e] is [v]. *)

  (** {1:low Low level functions}

      {b XXX.} Maybe move that to {!Ev} *)

  val instruct : ?propagate:bool -> ?default:bool -> 'a Ev.t -> unit
  (** [instruct ?propagate ?default e] defines the propagation and
      default behaviour of [e] according to [propagate] (defaults
      to [true]) and default (defaults to [true]). *)

  val listen :
    ?capture:bool -> ?propagate:bool -> ?default:bool ->
    Brr.Ev.target -> 'a Brr.Ev.type' -> ('a Brr.Ev.t -> unit) -> (unit -> unit)

  val endless_listen :
      ?capture:bool -> ?propagate:bool -> ?default:bool ->
      Brr.Ev.target -> 'a Brr.Ev.type' -> ('a Brr.Ev.t -> unit) -> unit
end

(** Reactive DOM elements.

    {b Warning.} Reactive DOM element mutators ({!set_at},
    {!set_children}, etc.) and definers ({!def_at}, {!def_children},
    etc.) use {{!Note.Logr}[Note] loggers} to perform their action.
    To prevent memory leaks, these loggers, and thus their action,
    automatically get destroyed whenever the element is removed from
    the HTML DOM. *)
module Elr : sig

  (** {1:children Children} *)

  val set_children : El.t -> on:El.t list event -> unit
  (** [set_children e ~on] sets [e]'s children with the value of [on]
      whenever it occurs. *)

  val def_children : El.t -> El.t list signal -> unit
  (** [def_children e cs] defines [e]'s children over time with the
      value of signal [cs]. {b Warning.} This assumes [cs] is the only
      entity interacting with the children. *)

  (** {1:ats Attributes and properties} *)

  val set_at : At.name -> on:Jstr.t option event -> El.t -> unit
  (** [set_at a ~on e] sets attribute [a] of [e] with the value
      of [e] whenever it occurs. If the value is [None] this removes
      the attribute. *)

  val def_at : At.name -> Jstr.t option signal -> El.t -> unit
  (** [def_at a v e] defines the attribute [a] of [e] over time
      with the value of [v]. Whenever the signal value is [None],
      the attribute is removed. {b Warning.} This assumes [v] is the
      only entity interacting with that attribute. *)

  val set_prop : 'a El.Prop.t -> on:'a event -> El.t -> unit
  (** [set_prop p ~on e] sets property [p] of [e] to the value
      of [on] whenever it occurs. *)

  val def_prop : 'a El.Prop.t -> 'a signal -> El.t -> unit
  (** [def_prop p v e] defines the property [p] of [e] over time with
      the value of [v]. {b Warning.} This assumes [v] is the only
      entity interacting with that property. *)

  (** {1:classes Classes} *)

  val set_class : Jstr.t -> on:bool event -> El.t -> unit
  (** [set_class a ~on e] sets the membership of [e] to class [e]
      with the value of [on] whenever it occurs. *)

  val def_class : Jstr.t -> bool signal -> El.t -> unit
  (** [rdef_class a b e] defines the membership of [e] to class [e]
      over time with the value of [b]. {b Warning.} This assumes [b] is
      the only entity interacting with that class. *)

  (** {1:styles Style} *)

  val set_inline_style :
    ?important:bool -> El.Style.prop -> on:Jstr.t event -> El.t -> unit
  (** [set_style ~important p ~on e] sets the inline style property [p]
      of [e] to the value of [on] whenever it occurs with priority
      [important] (defaults to [false]). *)

  val def_inline_style :
    ?important:bool -> El.Style.prop -> Jstr.t signal -> El.t -> unit
  (** [def_style p v e] sets the inline style property [p] of [e] over time
      with the value of [v]. {b Warning.} This assumes [v] is the only
      entity interacting with that property. *)

  (** {1:focus Focus} *)

  val set_has_focus : on:bool event -> El.t -> unit
  (** [set_focus e ~on] sets [e]'s focus with the value of [on]
      whenever it occurs. *)

  val def_has_focus : bool signal -> El.t -> unit
  (** [def_focus e v] defines the focus of [e] over time
      with the value of [v]. {b Warning.} This asumes [v] is the only
      entity interacting with [e]'s focus. *)

  (** {1:life_cycle Life-cycle callbacks}

      The browser document is watched for changes via a global
      {{:https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver}
      MutationObserver}. Whenever an element is added in the HTML DOM,
      its {!on_add} callbacks get called and disposed. Whenever an
      element is removed from the HTML DOM, {!on_rem} callbacks get
      called and disposed. A element is deemed part of the HTML DOM if
      its root node is the browser document. *)

  val on_add : (unit -> unit) -> El.t -> unit
  (** [on_add f e] references [f] until [e] is inserted in
      the HTML DOM, at which point [f ()] is invoked. *)

  val on_rem : (unit -> unit) -> El.t -> unit
  (** [on_rem f e] references [f] until [e] is removed from
      the HTML DOM, at which point [f ()] is invoked. *)

  (** {1:note Note loggers} *)

  val call : ('a -> El.t -> unit) -> on:'a event -> El.t -> unit
  (** [call f ~on e] calls [f] on [e] with the value of [e] whenever
      [on] occurs. The underlying logger is held by [e]. *)

  val hold_logr : El.t -> Logr.t -> unit
  (** [hold_logr e l] lets [e] hold logger [l] and destroy it via
      {!on_rem} once [e] is removed from the document. *)

  val may_hold_logr : El.t -> Logr.t option -> unit
  (** [may_hold_logr e l] is like {!hold_logr} but does nothing on
      [None]. *)
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
