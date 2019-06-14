(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Reactive browser interaction.

    {b TODO.}
    {ul
    {- It would be nice to get rid of Js.t types. We need to
       make a type to represent eventTarget and have [to_event_target]
       functions (e.g. on [Win.t])}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Js_of_ocaml
open Note

(** {1:std Preliminaries} *)

(** JavaScript strings.

    {b TODO.} do a useful string API. *)
module Jstr : sig

  (** {1:jstring Strings} *)

  type t
  (** The type for JavaScript strings. *)

  val v : string -> t
  (** [v s] is the string [s] as a javascript string. *)

  val vf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [vf fmt ...] is a Javascript string formatted according to [fmt]. *)

  val empty : t
  (** [empty] is an empty string. *)

  val is_empty : t -> bool
  (** [is_empty s] is [true] iff [s] is an empty string. *)

  val append : t -> t -> t
  (** [append s0 s1] appends [s1] to [s0]. *)

  val cuts : sep:t -> t -> t list
  (** [cuts sep s] is the list of all (possibly empty) substrings of
      [s] that are delimited by matches of the non empty separator
      string [sep]. *)

  val concat : sep:t -> t list -> t
  (** [concat sep ss] is the concatenates the list of strings [ss]
      inserting [sep] between each of them. *)

  val slice : ?start:int -> ?stop:int -> t -> t
  (** [slice ~start ~stop s] is the string s.[start], s.[start+1], ...
      s.[stop - 1]. [start] defaults to [0] and [stop] to
      [String.length s].

      If [start] or [stop] are negative they are subtracted from
      [String.length s]. This means that [-1] denotes the last
      character of the string. *)

  val trim : t -> t
  (** [trim s] is [s] without whitespace from the beginning and end of
      the string. *)

  val chop : prefix:t -> t -> t option
  (** [chop prefix s] is [s] without the prefix [prefix] or [None] if
      [prefix] is not a prefix of [s]. *)

  val rchop : suffix:t -> t -> t option
  (** [rchop suffix s] is [s] without the suffix [suffix] or [None] if
      [suffix] is not a suffix of [s]. *)

  val equal : t -> t -> bool
  (** [equal s0 s1] is [true] iff [s0] and [s1] are equal. *)

  val compare : t -> t -> int
  (** [compare s0 s1] is a total order on strings compatible with {!equal}. *)

  (** {1:conv Conversions} *)

  val to_string : t -> string
  (** [to_string s] is [s] as an OCaml string. *)

  val of_string : string -> t
  (** [of_string s] is the OCaml string [s] as a JavaScript string. *)

  val to_int : t -> int option
  (** [to_int s] is [int_of_string (to_string s)] with [Failure _] mapping
      on [None]. *)

  val of_int : int -> t
  (** [of_int i] is [Jstr.v (string_of_int i)]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] prints [s] on [ppf]. *)
end

(** JavaScript properties. *)
module Prop : sig
  type 'a t
  (** The type for properties of type ['a]. *)

  val v : undefined:'a -> Jstr.t list -> 'a t
  (** [v p : prop_type t] is accessed via path [p] of type [prop_type].
      {b Warning.} Always constrain the type otherwise this is
      {!Obj.magic}. [p] must be non-empty. *)

  val get : 'a t -> _ Js.t -> 'a
  (** [get p o] is the property [p] of object [o] if defined and
      [p]'s undefined value otherwise. *)

  val set : 'a t -> 'a -> _ Js.t -> unit
  (** [set p v o] sets property [p] of object [o] to [v]. *)

  (** {1:predef Predefined properties}

      These properties all have an undefined value: [Jstr.empty] for strings,
      [false] for booleans. *)

  val checked : bool t
  val id : Jstr.t t
  val name : Jstr.t t
  val title : Jstr.t t
  val value : Jstr.t t
end

(** Console logging.

    The following functions log to the browser console. *)
module Log : sig

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
end

(** Debugging tools. *)
module Debug : sig

  (** {1 Debug} *)

  val enter : unit -> unit
  (** [enter ()] stops and enters the JavaScript debugger (if available). *)

  val pp_obj : Format.formatter -> < .. > Js.t -> unit
  (** [pp_obj ppf o] applies the method [toString] to object [o] and
      prints the the resulting string on [ppf]. *)

  val dump_obj : < .. > Js.t -> unit
  (** [dump_obj o] dumps object [o] on the browser console with level debug. *)

  (** {1 Tracing} *)

  val tick : Format.formatter -> 'a -> unit
  (** [tick] formats ["tick"] on any value. *)

  val pr : ('a, Format.formatter, unit) Pervasives.format -> 'a
  (** [pr fmt ...] unconditionally outputs on the browser console with
      level Debug. Use {!Log.debug} for level-based conditional output. *)

  val trace : ?pp:(Format.formatter -> 'a -> unit) -> string -> 'a -> 'a
  (** [trace ~pp v] traces with {!Log.debug} the value of [v] with
      [pp] (defaults to {!tick}) and identifier [id]. The function
      returns [v]. *)

  val etrace :
    ?obs:bool -> ?pp:(Format.formatter -> 'a -> unit) -> string -> 'a event ->
    'a event
  (** [etrace ~pp id e] traces [e]'s occurence with {!Log.debug}, [pp]
      (defaults to {!tick}) and identifier [id]. If [obs] is [true],
      the return value is [e] itself and the tracing occurs through a
      logger, this will prevent [e] from being garbage collected. If
      [obs] is [false], the return value is [e] mapped by a tracing
      identity. *)

  val strace :
    ?obs:bool -> ?pp:(Format.formatter -> 'a -> unit) -> string -> 'a signal ->
    'a signal
  (** [strace ~pp id s] traces [s]'s changes with {!Log.debug}, [pp]
      and identifier [id]. If [obs] is [true], the return value is [s]
      itself and the tracing occurs through a logger, this will
      prevent [s] from being gc'd. If [obs] is [false], the return
      value is [s] mapped by a tracing identity and using [s]'s
      equality function. *)
end

(** Monotonic time. *)
module Time : sig

  (** {1 Time span} *)

  type span = float
  (** The type for time spans, in seconds. FIXME move to ms. *)

  (** {1 Passing time} *)

  val elapsed : unit -> span
  (** [elapsed ()] is the number of seconds elapsed since the
      beginning of the program. *)

  (** {1 Tick events} *)

  val tick : span -> span event
  (** [tick span] is an event that occurs once in [span] seconds with
      the value [span - span'] where [span'] is the actual delay
      performed by the system.

      {b Note.} Since the system may introduce delays you cannot
      assume that two different calls to {!tick} will necessarily
      yield two non-simultaneous events. *)

  val delay : span -> (unit -> unit) -> unit
  (** [delay span f] calls [f] after [span] seconds. *)

  (** {1 Counters} *)

  type counter
  (** The type for time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting time from call time on. *)

  val counter_value : counter -> span
  (** [counter_value c] is the current counter value in seconds. *)

  (** {1 Pretty printing time} *)

  val pp_s : Format.formatter -> span -> unit
  (** [pp_s ppf s] prints [s] seconds in seconds. *)

  val pp_ms : Format.formatter -> span -> unit
  (** [pp_ms ppf s] prints [s] seconds in milliseconds. *)

  val pp_mus : Format.formatter -> span -> unit
  (** [pp_mus ppf s] prints [s] seconds in microseconds. *)
end

(** {1:dom DOM} *)

(** DOM element attributes. *)
module Att : sig

  (** {1:atts Attributes} *)

  type name = Jstr.t
  (** The type for attribute names. *)

  type t = name * Jstr.t
  (** The type for attributes. *)

  val add_if : bool -> t -> t list -> t list
  (** [add_if b a l] is [a :: l] if [b] is [true] and [l] otherwise *)

  val add_some : name -> Jstr.t option -> t list -> t list
  (** [add_some n o l] is [v n a :: l] if [o] is [Some a] and [l] otherwise. *)

  (** {1 Predefined attribute constructors and names}

      {b Convention.} Whenever an attribute name conflicts with an OCaml
      keyword we prime them, see for example {!class'}. *)

  (** Attribute names. *)
  module Name : sig
    val autofocus : name
    val checked : name
    val class' : name
    val disabled : name
    val for' : name
    val height : name
    val href : name
    val id : name
    val name : name
    val placeholder : name
    val src : name
    val tabindex : name
    val title : name
    val type' : name
    val value : name
    val width : name
  end

  val autofocus : t
  val checked : t
  val class' : Jstr.t -> t
  val disabled : t
  val for' : Jstr.t -> t
  val height : int -> t
  val href : Jstr.t -> t
  val id : Jstr.t -> t
  val name : Jstr.t -> t
  val placeholder : Jstr.t -> t
  val src : Jstr.t -> t
  val tabindex : int -> t
  val title : Jstr.t -> t
  val type' : Jstr.t -> t
  val value : Jstr.t -> t
  val width : int -> t
end

(** DOM elements.

    {b Warning.} Reactive DOM element mutators ({!rset_att},
    {!rset_children}, etc.) and definers ({!def_att}, {!def_children},
    etc.) use {{!Note.Logr}[Note] loggers} to perform their action.
    To prevent memory leaks, these loggers, and thus their action,
    automatically get destroyed whenever the element is removed from
    the HTML DOM. *)
module El : sig

  (** {1:elements Elements} *)

  type name = Jstr.t
  (** The type for element names. *)

  type el
  (** The type for elements. *)

  type t = [ `El of el ]
  (** The type for elements. *)

  type child = [ t | `Txt of Jstr.t ]
  (** The type for element children. *)

  val v : ?atts:Att.t list -> name -> child list -> [> t]
  (** [v ?atts name cs] is an element [name] with attribute [atts]
      (defaults to [[]]) and children [cs]. If [atts] specifies
      an attribute more than once, the last one takes over with
      the exception of {!Att.klass}, whose occurences accumulate to
      define the final value. *)

  val el : t -> Dom_html.element Js.t
  (** [el (`El e)] is [e]. *)

  val tag_name : t -> Jstr.t
  (** [tag_name e] is the
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/tagName}
      tag name} of the element. *)

  val find_id : Jstr.t -> [> t] option
  (** [find_id id] is the element with id [id] in the browser document
      (if any). *)

  val find_class : Jstr.t -> [> t] list
  (** [find_class cl] are the element with class [cs] in the browser
      document. *)

  val document : unit -> [> t]
  (** [document] is the document's element of the current browser
      document. *)

  val document_body : unit -> [> t]
  (** [document_body] is the {!body} element of the current browser
      document. *)

  val document_active : unit -> [> t] option
  (** [document_active] is the element of the DOM that currently
      has focus (if any). *)

  (** {1:children Children} *)

  val set_children : t -> child list -> unit
  (** [set_children e children] sets [e]'s children to [children] *)

  val rset_children : t -> on:child list event -> unit
  (** [rset_children e ~on] sets [e]'s children with the value of [on]
      whenever it occurs. *)

  val def_children : t -> child list signal -> unit
  (** [def_children e cs] defines [e]'s children over time with the
      value of signal [cs]. {b Warning.} This assumes [cs] is the only
      entity interacting with the children. *)

  (** {1:atts Attributes} *)

  val get_att : Att.name -> t -> Jstr.t option
  (** [get_att a e] is the value of the attribute [a] of [e] (if any). *)

  val set_att : Att.name -> Jstr.t option -> t -> unit
  (** [set_att a v e] sets the value of attribute [a] of [e] to [v].
      If [v] is [None] this removes the attribute. *)

  val rset_att : Att.name -> on:Jstr.t option event -> t -> unit
  (** [rset_att a ~on e] sets attribute [a] of [e] with the value
      of [e] whenever it occurs. If the value is [None] this removes
      the attribute. *)

  val def_att : Att.name -> Jstr.t option signal -> t -> unit
  (** [def_att a v e] defines the attribute [a] of [e] over time
      with the value of [v]. Whenever the signal value is [None],
      the attribute is removed. {b Warning.} This assumes [v] is the
      only entity interacting with that attribute. *)

  (** {1:classes Classes} *)

  val get_class : Jstr.t -> t -> bool
  (** [get_class c e] is the membership of [e] to class [c]. *)

  val set_class : Jstr.t -> bool -> t ->  unit
  (** [set_class c b e] sets the membership of [e] to class [c]
      according to [b]. *)

  val rset_class : Jstr.t -> on:bool event -> t -> unit
  (** [rset_class a ~on e] sets the membership of [e] to class [e]
      with the value of [on] whenever it occurs. *)

  val def_class : Jstr.t -> bool signal -> t -> unit
  (** [rdef_class a b e] defines the membership of [e] to class [e]
      over time with the value of [b]. {b Warning.} This assumes [b] is
      the only entity interacting with that class. *)

  (** {1:properies Properties} *)

  val get_prop : 'a Prop.t -> t -> 'a
  (** [get_prop p e] is the property [p] of element [e] if defined and
      [p]'s undefined value otherwise. *)

  val set_prop : 'a Prop.t -> 'a -> t -> unit
  (** [set_prop p v o] sets property [p] of element [e] to [v]. *)

  val rset_prop : 'a Prop.t -> on:'a event -> t -> unit
  (** [rset_prop p ~on e] sets property [p] of [e] to the value
      of [on] whenever it occurs. *)

  val def_prop : 'a Prop.t -> 'a signal -> t -> unit
  (** [def_prop p v e] defines the property [p] of [e] over time with
      the value of [v]. {b Warning.} This assumes [v] is the only
      entity interacting with that property. *)

  (** {1:style Style} *)

  (** Style property names. *)
  module Style : sig
    type prop = Jstr.t
    val background_color : prop
    val color : prop
    val cursor : prop
    (** See {!Mouse.Cursor} for property values. *)

    val display : prop
    val height : prop
    val visibility : prop
    val width : prop
  end

  val get_computed_style : Style.prop -> t -> Jstr.t
  (** [get_computed_style p e] is the computed style property [p] of [e]. *)

  val get_style : Style.prop -> t -> Jstr.t
  (** [get_style p e] is the inline style property [p] of [e]. *)

  val set_style : ?important:bool -> Style.prop -> Jstr.t -> t -> unit
  (** [set_style ~important p v e] sets the inline style property [p] of
      [e] to [v] with priority [important] (defaults to [false]). *)

  val rset_style :
    ?important:bool -> Style.prop -> on:Jstr.t event -> t -> unit
  (** [rset_style ~important p ~on e] sets the inline style property [p]
      of [e] to the value of [on] whenever it occurs with priority
      [important] (defaults to [false]). *)

  val def_style : ?important:bool -> Style.prop -> Jstr.t signal -> t -> unit
  (** [def_style p v e] sets the inline style property [p] of [e] over time
      with the value of [v]. {b Warning.} This assumes [v] is the only
      entity interacting with that property. *)

  (** {1:focus Focus} *)

  val set_focus : bool -> t -> unit
  (** [focus b e] sets the focus of [e] to [b]. *)

  val rset_focus : on:bool event -> t -> unit
  (** [rset_focus e ~on] sets [e]'s focus with the value of [on]
      whenever it occurs. *)

  val def_focus : bool signal -> t -> unit
  (** [def_focus e v] defines the focus of [e] over time
      with the value of [v]. {b Warning.} This asumes [v] is the only
      entity interacting with [e]'s focus. *)

  (** {1:click Click simulation} *)

  val click : t -> unit
  (** [click e] simulates a click on [e]. *)

  val select_txt : t -> unit
  (** [select_txt e] selects the textual contents of [e]. If the DOM
      element [e] has no [select] method this does nothing. *)

  (** {1:life_cycle Life-cycle callbacks}

      The browser document is watched for changes via a global
      {{:https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver}
      MutationObserver}. Whenever an element is added in the HTML DOM,
      its {!on_add} callbacks get called and disposed. Whenever an
      element is removed from the HTML DOM, {!on_rem} callbacks get
      called and disposed. A element is deemed part of the HTML DOM if
      its root node is the browser document. *)

  val on_add : (unit -> unit) -> t -> unit
  (** [on_add f e] references [f] until [e] is inserted in
      the HTML DOM, at which point [f ()] is invoked. *)

  val on_rem : (unit -> unit) -> t -> unit
  (** [on_rem f e] references [f] until [e] is removed from
      the HTML DOM, at which point [f ()] is invoked. *)

  (** {1:note Note loggers} *)

  val call : ('a -> t -> unit) -> on:'a event -> t -> unit
  (** [call f ~on e] calls [f] on [e] with the value of [e] whenever
      [on] occurs. The underlying logger is held by [e]. *)

  val hold_logr : t -> Logr.t -> unit
  (** [hold_logr e l] lets [e] hold logger [l] and destroy it via
      {!on_rem} once [e] is removed from the document. *)

  val may_hold_logr : t -> Logr.t option -> unit
  (** [may_hold_logr e l] is like {!hold_logr} but does nothing on
      [None]. *)

  (** {1:cons Element constructors}

      {b Convention.} Whenever an element name conflicts with an OCaml
      keyword we prime it, see for example {!object'}. *)

  (** Element names. *)
  module Name : sig
    val a : name
    val abbr : name
    val address : name
    val area : name
    val article : name
    val aside : name
    val audio : name
    val b : name
    val base : name
    val bdi : name
    val bdo : name
    val blockquote : name
    val body : name
    val br : name
    val button : name
    val canvas : name
    val caption : name
    val cite : name
    val code : name
    val col : name
    val colgroup : name
    val command : name
    val datalist : name
    val dd : name
    val del : name
    val details : name
    val dfn : name
    val div : name
    val dl : name
    val dt : name
    val em : name
    val embed : name
    val fieldset : name
    val figcaption : name
    val figure : name
    val footer : name
    val form : name
    val h1 : name
    val h2 : name
    val h3 : name
    val h4 : name
    val h5 : name
    val h6 : name
    val head : name
    val header : name
    val hgroup : name
    val hr : name
    val html : name
    val i : name
    val iframe : name
    val img : name
    val input : name
    val ins : name
    val kbd : name
    val keygen : name
    val label : name
    val legend : name
    val li : name
    val link : name
    val map : name
    val mark : name
    val menu : name
    val meta : name
    val meter : name
    val nav : name
    val noscript : name
    val object' : name
    val ol : name
    val optgroup : name
    val option : name
    val output : name
    val p : name
    val param : name
    val pre : name
    val progress : name
    val q : name
    val rp : name
    val rt : name
    val ruby : name
    val s : name
    val samp : name
    val script : name
    val section : name
    val select : name
    val small : name
    val source : name
    val span : name
    val strong : name
    val style : name
    val sub : name
    val summary : name
    val sup : name
    val table : name
    val tbody : name
    val td : name
    val textarea : name
    val tfoot : name
    val th : name
    val thead : name
    val time : name
    val title : name
    val tr : name
    val track : name
    val u : name
    val ul : name
    val var : name
    val video : name
    val wbr : name
  end

  type 'a cons = ?atts:Att.t list -> child list -> ([> t] as 'a)
  (** The type for element constructors. This is simply {!v} with
      a pre-applied element name. *)

  val a : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a}a} *)

  val abbr : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}abbr} *)

  val address : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address}
      address} *)

  val area : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}area} *)

  val article : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}
      article} *)

  val aside : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside}
      aside} *)

  val audio : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}
      audio} *)

  val b : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b}b} *)

  val base : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}base} *)

  val bdi : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi}bdi} *)

  val bdo : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo}bdo} *)

  val blockquote : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}
      blockquote} *)

  val body : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body}body} *)

  val br : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br}br} *)

  val button : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}
      button} *)

  val canvas : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}
      canvas} *)

  val caption : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}
      caption} *)

  val cite : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite}cite} *)

  val code : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code}code} *)

  val col : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col}col} *)

  val colgroup : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}
      colgroup} *)

  val command : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/command}
      command} *)

  val datalist : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist}
      datalist} *)

  val dd : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd}dd} *)

  val del : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del}del} *)

  val details : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}
      details} *)

  val dfn : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn}dfn} *)

  val div : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div}div} *)

  val dl : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl}dl} *)

  val dt : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt}dt} *)

  val em : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em}em} *)

  val embed : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}
      embed} *)

  val fieldset : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}
      fieldset} *)

  val figcaption : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption}
      figcaption} *)

  val figure : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure}
      figure} *)

  val footer : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer}
      footer} *)

  val form : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}form} *)

  val h1 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1}h1} *)

  val h2 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2}h2} *)

  val h3 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3}h3} *)

  val h4 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4}h4} *)

  val h5 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5}h5} *)

  val h6 : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6}h6} *)

  val head : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head}head} *)

  val header : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header}
      header} *)

  val hgroup : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hgroup}
      hgroup} *)

  val hr : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr}hr} *)

  val html : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html}html} *)

  val i : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i}i} *)

  val iframe : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}
      iframe} *)

  val img : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img}img} *)

  val input : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}
      input} *)

  val ins : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins}ins} *)

  val kbd : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd}kbd} *)

  val keygen : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}
      keygen} *)

  val label : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}
      label} *)

  val legend : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend}
      legend} *)

  val li : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li}li} *)

  val link : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}link} *)

  val map : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map}map} *)

  val mark : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}mark} *)

  val menu : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}menu} *)

  val meta : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}meta} *)

  val meter : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
      meter} *)

  val nav : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav}nav} *)

  val noscript : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript}
      noscript} *)

  val object' : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}
      object} *)

  val ol : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol}ol} *)

  val optgroup : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}
      optgroup} *)

  val option : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}
      option} *)

  val output : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}
      output} *)

  val p : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p}p} *)

  val param : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}
      param} *)

  val pre : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre}pre} *)

  val progress : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}
      progress} *)

  val q : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q}q} *)

  val rp : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp}rp} *)

  val rt : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt}rt} *)

  val ruby : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}ruby} *)

  val s : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s}s} *)

  val samp : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}samp} *)

  val script : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script}
      script} *)

  val section : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section}
      section} *)

  val select : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}
      select} *)

  val small : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small}
      small} *)

  val source : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}
      source} *)

  val span : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span}span} *)

  val strong : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong}
      strong} *)

  val style : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style}
      style} *)

  val sub : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub}sub} *)

  val summary : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary}
      summary} *)

  val sup : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup}sup} *)

  val table : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}
      table} *)

  val tbody : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}
      tbody} *)

  val td : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td}td} *)

  val textarea : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}
      textarea} *)

  val tfoot : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}
      tfoot} *)

  val th : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th}th} *)

  val thead : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}
      thead} *)

  val time : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}time} *)

  val title : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/title}
      title} *)

  val tr : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr}tr} *)

  val track : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}
      track} *)

  val u : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u}u} *)

  val ul : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul}ul} *)

  val var : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var}var} *)

  val video : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}
      video} *)

  val wbr : 'a cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr}wbr} *)
end

(** DOM events *)
module Ev : sig

  (** {1:events Events and event kinds} *)

  type 'a target = (#Dom_html.eventTarget as 'a) Js.t
  (** The type for event targets. *)

  type 'a kind = (#Dom_html.event as 'a) Js.t Dom_html.Event.typ
  (** The type for kind of events. See {!kinds}. *)

  type 'a t = (#Dom_html.event as 'a) Js.t
  (** The type for events. *)

  (** {1:note Events as [Note] events} *)

  val for_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'a target ->
    'b kind -> ('b t -> 'c) -> 'c event
  (** [for_target ~capture ~propagate ~default t k f] is an event that
      reports events of kind [k] transformed by [f] on target [t]. If
      [capture] is [true] the event occurs during the capture phase
      (defaults to [false]). If [propagate] is [true] (default) the
      event is propagated. The default action is performed iff
      [default] is [true] (defaults to the value of [propagate]). *)

  val for_targets :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'a target list ->
    'b kind -> ('a target -> 'b t -> 'c) -> 'c event
  (** {!for_targets} is like {!for_target} except the event occurs
      for the event kind on the given list of targets. *)

  val for_el :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> El.t ->
    'b kind -> ('b t -> 'c) -> 'c event
  (** [for_el] is like {!for_target} except the target is an element. *)

  val for_els :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> El.t list ->
    'b kind -> (El.t -> 'b t -> 'c) -> 'c event
  (** [for_els] is like {!for_targets} excepts the targets are elements. *)

  (** {2:evmap Event mappers} *)

  val ev : 'b t -> 'b t
  (** [ev e] is [e]. *)

  val unit : 'b t -> unit
  (** [unit e] is [()]. *)

  val stamp : 'a -> 'b t -> 'a
  (** [stamp v e] is [v]. *)

  (** {1:cb Event callbacks}

      Lower-level convenience interface. *)

  type cb
  (** The type for event callbacks. *)

  type cb_ret
  (** The type for callback return. *)

  val add_cb :
    ?capture:bool -> 'a target -> 'b kind -> ('a target -> 'b t -> cb_ret) -> cb
  (** [add_cb ~capture t k f] calls [f e ev] whenever an event [ev] of
      kind [k] occurs on [t]. If [capture] is [true] the callback occurs
      during the capture phase (defaults to [false]). *)

  val rem_cb : cb -> unit
  (** [rem_cb cb] removes the callback [cb]. *)

  val cb_ret : ?propagate:bool -> ?default:bool -> 'a t -> cb_ret
  (** [cb_ret ~propagate ~default ev] returns from the callback and
      propagates event [ev] if [propagate] is [true] (default). The
      default action is performed iff [default] is [true] (defaults to
      the value of [propagate]. *)

  (** {1:kinds Event kinds}

      MDN's web docs
      {{:https://developer.mozilla.org/en-US/docs/Web/Events}Event reference}.*)

  val kind : string -> 'a kind

  val abort : Dom_html.event kind
  val afterprint : Dom_html.event kind
  val beforeprint : Dom_html.event kind
  val beforeunload : Dom_html.event kind
  val blur : Dom_html.event kind
  val cached : Dom_html.event kind
  val change : Dom_html.event kind
  val click : Dom_html.event kind
  val dblclick : Dom_html.event kind
  val domContentLoaded : Dom_html.event kind
  val error : Dom_html.event kind
  val focus : Dom_html.event kind
  val hashchange : Dom_html.event kind
  val input : Dom_html.event kind
  val invalid : Dom_html.event kind
  val keydown : Dom_html.keyboardEvent kind
  val keypress : Dom_html.keyboardEvent kind
  val keyup : Dom_html.keyboardEvent kind
  val load : Dom_html.event kind
  val message : Dom_html.event kind
  val mousedown : Dom_html.mouseEvent kind
  val mouseenter : Dom_html.mouseEvent kind
  val mouseleave : Dom_html.mouseEvent kind
  val mousemove : Dom_html.mouseEvent kind
  val mouseout : Dom_html.mouseEvent kind
  val mouseover : Dom_html.mouseEvent kind
  val mouseup : Dom_html.mouseEvent kind
  val offline : Dom_html.event kind
  val online : Dom_html.event kind
  val pagehide : Dom_html.event kind
  val pageshow : Dom_html.event kind
  val popstate : Dom_html.event kind
  val progress : Dom_html.event kind
  val readystatechange : Dom_html.event kind
  val reset : Dom_html.event kind
  val resize : Dom_html.event kind
  val submit : Dom_html.event kind
  val unload : Dom_html.event kind
end

(** {1:ui User interaction} *)

(** User keyboard. *)
module Key : sig

  (** {1:keys Physical keys}

      {b Note.} Physical keys are for using the keyboard as a
      controller. They must not be used to derive text input: they are
      unrelated to the user's keyboard layout for text entry. *)

  type code = int
  (** The type for physical key codes. *)

  type t =
  [ `Alt of [ `Left | `Right ]
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Ascii of Char.t
  | `Backspace
  | `Ctrl of [ `Left | `Right ]
  | `End
  | `Enter
  | `Escape
  | `Func of int
  | `Home
  | `Insert
  | `Key of code
  | `Meta of [ `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Return
  | `Shift of [ `Left | `Right ]
  | `Spacebar
  | `Tab ]
  (**  The type for physical keys.

       {b Warning.} This type is overdefined for now. For example
       except for [`Meta], [`Left] and [`Right] modifiers cannot be
       distinguished; [`Left] is always returned. [`Enter] and
       [`Return] cannot be distinguished, [`Return] is always
       returned. *)

  val of_ev : Dom_html.keyboardEvent Ev.t -> t
  (** [of_ev e] is the physical key of keyboard event [e]. *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k0] and [k1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order on keys compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats keys. *)

  (** {1:ev Keyboard events} *)

  type events
  (** The type for gathering keyboard events on a given target. *)

  val for_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> 'a Ev.target -> events
  (** [for_target t] is keyboard events for target [t]. The other
      parameters are those of {!Ev.for_target}. *)

  val for_el :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> El.t -> events
  (** [for_el e] is like {!for_target} but for an element. *)

  (** {1:kev Key events} *)

  val any_down : events -> t event
  (** [any_down evs] occurs whenever a key goes down on the target. *)

  val any_up : events -> t event
  (** [any_down evs] occurs whenever a key goes up on the target. *)

  val any_holds : events -> bool signal
  (** [any_holds evs] is [true] whenever any key is down. *)

  val down : events -> t -> unit event
  (** [down evs k] occurs whenever key [k] goes down on the target. *)

  val up : events -> t -> unit event
  (** [up evs k] occurs whenever key [k] goes up on the target. *)

  val holds : events -> t -> bool signal
  (** [holds evs k] is [true] whenever [k] is held down on the target. *)

  (** {1:mods Modifiers signals} *)

  val alt : events -> bool signal
  (** [alt evs] is [true] whenver an alt key is down on the target.
      Equivalent to:
{[
S.Bool.(holds evs (`Alt `Left) || holds evs (`Alt `Right))
]} *)

  val ctrl : events -> bool signal
  (** [ctrl evs] is [true] whenver an ctrl key is down on the target.
      Equivalent to:
{[
S.Bool.(holds evs (`Ctrl `Left) || holds evs (`Ctrl `Right))
]} *)

  val meta : events -> bool signal
  (** [meta evs] is [true] whenver an meta key is down on the target.
      Equivalent to:
{[
S.Bool.(holds evs (`Meta `Left) || holds evs (`Meta `Right))
]} *)

  val shift : events -> bool signal
  (** [shift evs] is [true] whenver an shift key is down on the target.
      Equivalent to:
{[
S.Bool.(holds evs (`Meta `Left) || holds evs (`Meta `Right))
]} *)

(** {1:semantics Semantic incoherences}

    {!holds} and {!any_holds} may be initially set to [false] even
    though they should be [true] if {!for_target} is invoked when the
    corresponding keys are depressed. *)

(** {1:repeat Key repeat events}

    Key repeat events are not exposed. There are two main use cases
    for key repeat. First during text input, but his should be handled
    by text input events and is out of scope. Second for controlling
    changes to a variable over time, e.g. scrolling with a keyboard.
    In the latter case it is better to create a timing signal or event
    with a known rate while the key is held. *)
end

(** User mouse.

    Excepts for mouse ups, mouse events are only reported whenever the
    mouse is over the specified target.

    By default coordinates are in target normalized coordinates with
    (0, 0) corresponding to the bottom left corner and (1,1) to the
    top right corner. *)
module Mouse : sig

  (** {1:mouse Mouse events} *)

  val pt : float -> float -> float * float
  (** [pt x y] is [(x, y)]. *)

  type 'a events
  (** The type for gathering mouse events on a given target and using
      ['a] to represent points. *)

  val for_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> ?normalize:bool ->
    'b Ev.target -> (float -> float -> 'a) -> 'a events
  (** [for_target t pt] is mouse events for target [t] using [pt] to
      construct points. If [normalize] is [true] (default) coordinates
      are reported in target normalized coordinates (see above), if
      [false] they are reported in pixels with the origin at the
      top-left of the element. The other parameters are those from
      {!Ev.for_target}. *)

  val for_el :
    ?capture:bool -> ?propagate: bool -> ?default:bool -> ?normalize:bool ->
    El.t -> (float -> float -> 'a) -> 'a events
  (** [for_el] is like {!for_target} but for an element. Note that
      {!destroy} automatically gets called with the result whenever
      the element is removed from the HTML DOM. *)

  val destroy : 'a events -> unit
  (** [destroy evs] removes the callbacks registred by [evs]. It's
      important to perform this whenever you no longer need the events
      as [evs] needs to register callbacks with the document to
      correctly capture mouse ups. *)

  (** {1 Mouse position} *)

  val pos : 'a events -> 'a signal
  (** [pos evs] is the current mouse position in the target. *)

  val dpos : 'a events -> 'a event
  (** [dpos evs] occurs on mouse moves with current mouse position minus
      the previous position. *)

  val mem : 'a events -> bool signal
  (** [mem evs] is [true] whenever the mouse position is inside
      the target. *)

  (** {1 Mouse buttons} *)

  val left : 'a events -> bool signal
  (** [left evs] is [true] whenever the left mouse button went down in
      the target and did not go up yet. *)

  val left_down : 'a events -> 'a event
  (** [left_down evs] has an occurence with the mouse position
      whenever the button goes down in the target. *)

  val left_up : 'a events -> 'a event
  (** [left_up evs] is [true] whenever the left mouse button went down
      in the target and goes back up {e anywhere}. Note that the reported
      position might not be in the target. *)

  val mid : 'a events -> bool signal
  (** [mid] is like {!left} but the middle button. *)

  val mid_down : 'a events -> 'a event
  (** [mid_down]is like {!left_down} but for the middle button. *)

  val mid_up :'a events -> 'a event
  (** [mid_up] is like {!left_up} but for the middle button. *)

  val right : 'a events -> bool signal
  (** [right] is like {!left} but the right button. *)

  val right_down : 'a events -> 'a event
  (** [right_down]is like {!left_down} but for the right button. *)

  val right_up :'a events -> 'a event
  (** [right_up] is like {!left_up} but for the right button. *)

  (** {1:cursors Mouse cursors} *)

  (** Mouse cursors.

      To be used with {!El.Style.cursor}. *)
  module Cursor : sig

    (** {1:cursors Mouse cursors} *)

    type t = Jstr.t
    (** The type for specifying cusrors. *)

    val url : ?x:int -> ?y:int -> string -> t
    (** [url ~x ~y u] is an image cursor using URL [u] for the image
        with [x,y] identifiying the position of the hotspot in the image
        relative to the top-left corner (defaults to [0,0]). *)

    (** {1:gen General purpose cursors} *)

    val auto : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-auto}auto} *)

    val default : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-default}default} *)

    val none : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-none}none} *)

    (** {1:links_status Links and status} *)

    val context_menu : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-context-menu}
        context-menu} *)

    val help : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-help}help} *)

    val pointer : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-pointer}pointer} *)

    val progress : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-progress}progress} *)

    val wait : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-wait}wait} *)

    (** {1:sel Selection cursors} *)

    val cell : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-cell}cell} *)

    val crosshair : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-crosshair}crosshair} *)

    val text : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-text}text} *)

    val vertical_text : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-vertical-text}
        vertical-text} *)

    (** {1:dd Drag and drop cursors} *)

    val alias : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-alias}alias} *)

    val copy : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-copy}copy} *)

    val move : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-move}move} *)

    val no_drop : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-no-drop}no-drop} *)

    val not_allowed : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-not-allowed}
        not-allowed} *)

    val grab : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-grab}grab} *)

    val grabbing : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-grabbing}grabbing} *)

    (** {1:resize_scroll Resizing and scrolling cursors} *)

    val e_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-e-resize}e-resize} *)

    val n_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-n-resize}n-resize} *)

    val ne_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-ne-resize}ne-resize} *)

    val nw_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-nw-resize}nw-resize} *)

    val s_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-s-resize}s-resize} *)

    val se_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-se-resize}se-resize} *)

    val sw_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-sw-resize}sw-resize} *)

    val w_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-w-resize}w-resize} *)

    val ew_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-ew-resize}ew-resize} *)

    val ns_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-ns-resize}ns-resize} *)

    val nesw_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-nesw-resize}
        nesw-resize} *)

    val nwse_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-nwse-resize}
        nwse-resize} *)

    val col_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-col-resize}
        col-resize} *)

    val row_resize : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-row-resize}
        row-resize} *)

    val all_scroll : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-all-scroll}
        all-scroll} *)

    (** {1:zooming_cursors Zooming cursors} *)

    val zoom_in : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-zoom-in}
        zoom-in} *)

    val zoom_out : t
    (** {{:https://www.w3.org/TR/css-ui-3/#valdef-cursor-zoom-out}
        zoom-out} *)
  end
end


(** Human factors. *)
module Human : sig

  (** {1 System latency feelings}

      These values are from
      {{:http://www.nngroup.com/articles/response-times-3-important-limits/}
      here}. *)

  val noticed : Time.span
  (** [noticed] is [0.1]s, the time span after which the user will
      notice a delay and feel that the system is not reacting
      instantaneously. *)

  val interrupted : Time.span
  (** [interrupted] is [1.]s, the time span after which the user will
      feel interrupted and feedback from the system is needed. *)

  val left : Time.span
  (** [left] is [10.]s, the time span after which the user will
      switch to another task and feedback indicating when the system
      expects to respond is needed. *)

  val feel : unit -> [ `Interacting | `Interrupted | `Left ] signal
  (** [feel ()] is a signal that varies according to user latency
      constants:
      {ul
      {- \[[user_feel ()]\]{_t} [= `Interacting] if
         [t < User.interrupted].}
      {- \[[user_feel ()]\]{_t} [= `Interrupted] if
         [t < User.left].}
      {- \[[user_feel ()]\]{_t} [= `Left] if [t >= User.left].}} *)

  (** {1 Touch target and finger sizes}

      These values are from
      {{:http://msdn.microsoft.com/en-us/library/windows/apps/hh465415.aspx#touch_targets}here}. *)

  val touch_target_size : float
  (** [touch_target_size] is [9.]mm, the recommended touch target size in
      millimiters. *)

  val touch_target_size_min : float
  (** [touch_size_min] is [7.]mm, the minimal touch target size in
      millimeters. *)

  val touch_target_pad : float
  (** [touch_target_pad] is [2.]mm, the minimum padding size in
      millimeters between two touch targets. *)

  val average_finger_width : float
  (** [average_finger_width] is [11.]mm, the average {e adult} finger width. *)
end

(** Application. *)
module App : sig

  (** {1:environment Environment} *)

  val env : string -> default:'a -> (string -> 'a) -> 'a
  (** [env var ~default parse] lookups [var] in the environment,
      parses it with [parse] and returns the result. Lookups th the
      query string of [window.location] for the first matching
      [var=value] pair. *)

  (** {1:fullscreen Fullscreen} *)

  val fullscreen_available : bool
  (** [fullscreen_enabled] is [true] [iff] fullscreen is available. *)

  val request_fullscreen : El.t -> unit
  (** [request_fullscreen e] requests [e] to take up the entire screen. *)

  val exit_fullscreen : unit -> unit
  (** [exit_fullscreen ()] exits fullscreen mode. *)

  val is_fullscreen : bool signal
  (** [is_fullscreen] is [true] iff the application is in fullcreen mode. *)

  (** {1:userquit User requested quit} *)

  val quit : unit event
  (** [quit] occurs whenever the user requested to quit. The browser window
      is closing and it's your last chance to peform something. *)

  (** {1 Run} *)

  val run : ?name:string -> (unit -> unit) -> unit
end

(** {1:browser Browser} *)

(** Browser window. *)

(** Browser window. *)
module Win : sig
  type t = Dom_html.window Js.t
  (** The type for browser windows. *)

  val dow : t
  (** [dow] is the browser window. *)

  val device_pixel_ratio : t -> float
  (** [device_pixel_ratio w] is the ratio between physical and CSS pixel
      resolution. A value of [2.] indicates that two physical pixels are
      used to draw a single CSS pixel. *)

  val print : t -> unit
  (** [print w] opens the print dialog to print the current document. *)
end

(** Browser location

    {b TODO.} Needs redesign/review.

    {b Warning.} We use the terminology and return data according to
    {{:http://tools.ietf.org/html/rfc3986}RFC 3986},
    not according to the broken HTML URLUtils interface.  *)
module Loc : sig

  (** {1:info Location URI} *)

  val uri : unit -> Jstr.t
  (** [uri ()] is the browser's location full URI. *)

  val scheme : unit -> Jstr.t
  (** [scheme ()] is the scheme of {!uri}[ ()]. *)

  val host : unit -> Jstr.t
  (** [host ()] is the host of {!uri}[ ()]. *)

  val port : unit -> int option
  (** [port ()] is the port of {!uri}[ ()]. *)

  val path : unit -> Jstr.t
  (** [path ()] is the path of {!uri}[ ()]. *)

  val query : unit -> Jstr.t
  (** [query ()] is the query of {!uri}[ ()]. *)

  val fragment : unit -> Jstr.t
  (** [fragment ()] is fragment of {!uri}[ ()] with the hash. *)

  val set_fragment : Jstr.t -> unit
  (** [set_fragment frag] sets the fragment of {!uri}[ ()] to [frag].
      This does not reload the page but triggers the {!Ev.hashchange}
      event. *)

  val update :
    ?scheme:Jstr.t -> ?host:Jstr.t -> ?port:int option -> ?path:Jstr.t ->
    ?query:Jstr.t -> ?fragment:Jstr.t -> unit -> unit
  (** [update ~scheme ~hostname ~port ~path ~query ~fragment ()] updates the
      corresponding parts of the location's URI. *)

  (** {1:info Location changes} *)

  val hashchange : Jstr.t event
  (** [hashchange] occurs whenever the window's fragment changes with
      the new value of [fragment ()]. *)

  val set : ?replace:bool -> Jstr.t -> unit
  (** [set replace uri] sets to browser location to [uri], if
      [replace] is [true] the current location is removed from the
      session history (defaults to [false]). *)

  val reload : unit -> unit
  (** [reload ()] reloads the current location. *)
end

(** Browser history.

    {b TODO.} Needs redesign/review. *)
module History : sig

  (** {1 Moving in history} *)

  val length : unit -> int
  (** [length ()] is the number of elements in the history including
      the currently loaded page. *)

  val go : int -> unit
  (** [go step] goes [step] numbers forward (positive) or backward (negative)
      in history. *)

  val back : unit -> unit
  (** [back ()] is [go ~-1]. *)

  val forward : unit -> unit
  (** [forward ()] is [go 1]. *)

  (** {1 History state}

      {b Warning.} History state is unsafe if you don't properly version
      you state. Any change in state representation should entail a new
      version. *)

  type 'a state
  (** The type for state. *)

  val create_state : version:Jstr.t -> 'a -> 'a state
  (** [create_state version v] is state [v] with version [version]. *)

  val state : version:Jstr.t -> default:'a -> unit -> 'a
  (** [state version deafult ()] is the current state if it matches
      [version]. If it doesn't match or there is no state [default] is
      returned. *)

  (** {1 Making history} *)

  val push :
    ?replace:bool -> ?state:'a state -> title:Jstr.t -> Jstr.t -> unit
  (** [push ~replace ~state ~title uri] changes the browser location to
      [uri] but doesn't load the URI. [title] is a human title for the
      location to which we are moving and [state] is a possible value
      associated to the location. If [replace] is [true] (defaults to
      [false]) the current location is replaced rather than added to
      history. *)
end

(** Browser information *)
module Info : sig

  val languages : unit -> Jstr.t list
  (** [languages ()] is the user's preferred languages as BCP 47 language
      tags.

      FIXME support languagechange event on window. *)
end

(** Persistent storage.

    Persisent key-value store implemented over
    {{:http://www.w3.org/TR/webstorage/}webstorage}. Safe if no one
    tampers with the storage outside of the program. FIXME/TODO this
    still relies on the jsoo representation, add safer keys with type
    indexed codecs. *)
module Store : sig

  (** {1 Storage scope} *)

  type scope = [ `Session | `Persist ]
  (** The storage scope. *)

  val support : scope -> bool
  (** [support scope] is [true] iff values can be stored in [scope]. *)

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

  (** {1 Versioning} *)

  val force_version : ?scope:scope -> string -> unit
  (** [force_version v] checks that the version of the store is [v].  If
      it's not it {!clear}s the store and sets the version to [v]. *)
end

(** File system interaction. *)
module File : sig

  type t
  (** The type for file objects. *)

  val name : t -> Jstr.t
  (** [name f] is the file name of [f]. *)

  val size : t -> int
  (** [size f] is the file size of [f] *)

  val last_modified_ms : t -> int
  (** [last_modified_ms f] is the last modified time in ms from
      the epoch. *)

  val type' : t -> Jstr.t
  (** [type f] is the MIME type of [f]. *)

  val list_of_el : El.t -> t list
  (** [list_of_el e] is the file list of element [e]. *)

  (** Reading files. *)
  module Read : sig

    (** {1:progress Read progress} *)

    type progress
    (** The type for read progress. *)

    val bytes_read : progress -> int
    (** [bytes_read p] is the number of bytes read so far. *)

    val bytes_total : progress -> int option
    (** [bytes_total p] is the total number of bytes to read. *)

    (** {1:reads File reads} *)

    type error =
      [ `Aborted | `Security | `Not_readable | `Not_found | `Other of string ]
    (** The type for file read errors. *)

    val pp_error : Format.formatter -> error -> unit
    (** [pp_error ppf e] prints an unspecified representation of [e]
        on [ppf]. *)

    type file = t
    (** The type for files. *)

    type 'a t
    (** The type for file reads returning results of type ['a]. *)

    val to_data_url : file -> Jstr.t t
    (** [to_data_url f] reads file [f] as as a
        {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs}data URL}. This doesn't start the read use {!read}. *)

    val abort : 'a t -> unit
    (** [abort r] aborts the file read [r]. *)

    val result : 'a t -> (file * ('a, error) result) event
    (** [result r] is an event that occurs only once with the read
        result. *)

    val progress : 'a t -> (file * progress) event
    (** [progress r] is an event that occurs during the read of [r]. *)
  end
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
