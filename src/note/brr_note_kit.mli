(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Brr
open Note

(** {1:ui User interaction} *)

(** Window reactions. *)
module Windowr : sig

  (** {1:fullscreen Fullscreen} *)

  val is_fullscreen : bool signal
  (** [is_fullscreen] is [true] iff the application is in fullcreen mode. *)

  (** {1:userquit User requested quit} *)

  val quit : unit event
  (** [quit] occurs whenever the user requested to quit. The browser window
      is closing and it's your last chance to peform something. *)
end

(** User keyboard. *)
module Key : sig

  (** {1:keys Physical keys}

      {b Note.} Physical keys are for using the keyboard as a {e
      controller}. Do not use them to derive text input, they are
      unrelated to the user's keyboard layout for text entry. Use
      {{!Brr.Ev.input_events}input events} for text entry. *)

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
       except for [`Shift], [`Ctrl] and [Alt], [`Left] and [`Right]
       modifiers cannot be distinguished; [`Left] is always returned.
       [`Enter] and [`Return] cannot be distinguished, [`Return] is
       always returned. *)

  val of_ev : Ev.Keyboard.t Ev.t -> t
  (** [of_ev e] is the physical key of the keyboard event [e]. *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k0] and [k1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order on keys compatible with {!equal}. *)

  val to_jstr : t -> Jstr.t

  (** {1:ev Keyboard events} *)

  type events
  (** The type for gathering keyboard events on a given target. *)

  val on_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> Ev.target -> events
  (** [on_target t] is keyboard events for target [t]. The other
      parameters are those of {!Brr_note.Evr.on_target}. *)

  val on_el :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> El.t -> events
  (** [on_el e] is like {!on_target} but for an element. *)

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

  val on_target :
    ?capture:bool -> ?propagate:bool -> ?default:bool -> ?normalize:bool ->
   (float -> float -> 'a) ->  Ev.target -> 'a events
  (** [on_target pt t] is mouse events for target [t] using [pt] to
      construct points. If [normalize] is [true] (default) coordinates
      are reported in target normalized coordinates (see above), if
      [false] they are reported in pixels with the origin at the
      top-left of the element. The other parameters are those from
      {!Brr_note.Evr.on_target}. *)

  val on_el :
    ?capture:bool -> ?propagate: bool -> ?default:bool -> ?normalize:bool ->
    (float -> float -> 'a) -> El.t -> 'a events
  (** [on_el] is like {!on_target} but for an element. Note that
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

    val url : ?x:int -> ?y:int -> Jstr.t -> t
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

  val to_jstr : [`S | `Ms | `Mus] -> span -> Jstr.t
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
      {{:https://docs.microsoft.com/en-us/windows/win32/uxguide/inter-touch#control-sizes-and-touch-targeting}here}. *)

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

(** Graphical user interaction.

    {b Warning.} This will definitively break in the future.

    Note based GUI toolkit.

    {%html: <img src="../../_assets/note_ui_sample.png"
             style="width:450px;"/> %}

    {b XXX.}
    {ul
    {- A common interface seems to emerge. Can we get rid of the
    different types and unify the elements under a single type ?}
    {- Provide action refinement. This could be as an optional
       argument but it's likely you want to do it after. So
       as *.with_action functions seem better.}
    {- Layout is still painful}} *)
module Ui : sig

(** Element groups.

    Groups allow to gather and layout GUI elements and summarize
    their actions. See the {{!style}styling information}. *)
module Group : sig

  (** {1:group Groups} *)

  type dir = [ `H | `V ]
  (** The type for specifiy the layout direction. *)

  type align = [ `Start | `End | `Center | `Justify | `Distribute | `Stretch ]
  (** The type for specifying alignements. [`Stretch] is dodgy. *)

  type 'a t
  (** The type for groups which summarize actions of type ['a]. *)

  val v :
    ?class':Jstr.t -> ?enabled:bool signal -> ?action:'a event ->
    ?xdir_align:align -> ?dir_align:align -> dir:dir -> El.t list signal ->
    'a t
  (** [v ~class' ~enabled ~action ~dir_align ~xdir_align ~dir cs]
      layouts elements [cs] in a container. Arguments are as follows:
      {ul
      {- [dir] is the layout direction for elements}
      {- [dir_align] is the alignment between elements in the layout
         direction. Defaults to [`Start].}
      {- [xdir_align] is the alignement between elements in the direction
         perpendicular to the layout direction. Defaults to [`Start].}
      {- [action] can be used by the client to summarize the user interaction
         performed by the underlying elements. Defaults to {!E.enver}}
      {- [enabled] visually indicates if the group can be
         interacted with. Defaults to {!S.Bool.true'}}
      {- [class'] is added to the underlying element's classes.}} *)

  val dir : 'a t -> dir
  (** [dir g] is [g]'s children layout direction. *)

  val dir_align : 'a t -> align
  (** [dir_align g] is [g]'s children alignement along the layout direction. *)

  val xdir_align : 'a t -> align
  (** [xdir_align g] is [g]'s children alignement in the parti the direction. *)

  val action : 'a t -> 'a event
  (** [action g] occurs whenever an action occurs in the group (see {!v}). *)

  val enabled : 'a t -> bool signal
  (** [enabled g] is [true] iff [g] is enabled. *)

  val el : 'a t -> El.t
  (** [el b] is [b]'s DOM element. *)

  (** {1:tr Transforming UI elements} *)

  val with_action : 'b event -> 'a t -> 'b t
  (** [with_action action g] uses [g] for [g]'s action. *)

  val hide_action : 'b t -> 'a t
  (** [hide_action g] is [with_action E.never g]. *)

  (** {1:style Styling}

      The element returned by {!el} makes use of the following CSS
      classes:
      {ul
      {- [ui-group] always.}
      {- [ui-dir-{h,v}] according to {!dir}.}
      {- [ui-dir-align-{start,end,center,justify,distribute,stretch}] according
         to {!dir_align}}
      {- [ui-xdir-align-{start,end,center,justify,distribute,stretch}] according
         to {!xdir_align}}
      {- [ui-disabled] whenever {!enabled} is [false].}} *)
end

(** Labels.

    Labels are for displaying short units of textual content. See
    the {{!style}styling information}. *)
module Label : sig

  (** {1:labels Labels} *)

  type t
  (** The type for labels. *)

  val v :
    ?class':Jstr.t -> ?enabled:bool signal -> ?tip:Jstr.t signal ->
    El.t list signal -> t
  (** [v ~class' ~enabled ~tip label] is a label with:
      {ul
      {- [label] the label's contents.}
      {- [enabled] indicates if the label should look as such. Defaults to
         {!S.Bool.true'}}
      {- [tip] is a tooltip for the label.}
      {- [class'] is added to the element's classes.}} *)

  val enabled : t -> bool signal
  (** [enabled l] is a signal that is [true] iff the label is enabled. *)

  val el : t -> El.t
  (** [el l] is [l]'s DOM element. *)

  (** {1:style Styling}

      The element returned by {!el} makes use of the following CSS
      classes:
      {ul
      {- [ui-label] always.}
      {- [ui-disabled] whenever {!enabled} is [false].}} *)
end

(** Buttons.

    See the {{!style}styling information}. *)
module Button : sig

  (** {1:buttons Buttons} *)

  type 'a t
  (** The type for buttons whose action occurs with type ['a]. *)

  val v :
    ?class':Jstr.t -> ?active:bool signal -> ?enabled:bool signal ->
    ?tip:Jstr.t signal -> El.t list signal -> 'a -> 'a t
  (** [v ~class' ~active ~enabled ~tip label action] is a button with:
      {ul
      {- [label] the button's label.}
      {- [action] the value reported when the button is actuated.}
      {- [tip] a tooltip for the button.}
      {- [enabled] indicates if the button can
          be interacted with. Defaults to [S.Bool.true'].}
      {- [active] indicates that the button is
          being interacted with programmatically (e.g. via a
          shortcut)}
      {- [class'] is added to the underlying element's classes}} *)

  val action : 'a t -> 'a event
  (** [action b] is an event that occurs when the button is actuated. *)

  val enabled : 'a t -> bool signal
  (** [enabled b] is a signal that is [true] iff the button is enabled. *)

  val active : 'a t -> bool signal
  (** [active b] is a signal that is [true] iff the button is
      being interacted with. {b FIXME.} For now this doesn't change on
      mouse activation. *)

  val el : 'a t -> El.t
  (** [el b] is [b]'s DOM element. *)

  (** {1:special Special buttons} *)

  val file_selector :
    ?class':Jstr.t -> ?active:bool signal -> ?enabled:bool signal ->
    ?tip:Jstr.t signal -> ?exts:string list -> El.t list signal ->
    File.t t
  (** [file_selector ~exts cs] is a button which, whenever clicked,
      allows to select a file on the host's file system. [exts] is the
      caseless list of file extensions (including the dot) that
      can be selected; all file can be selected if this is the empty
      list (default).

      The resulting {{!el}element} as an additional [ui-file-selector]
      class. See {!style}. *)

  val files_selector :
    ?class':Jstr.t -> ?active:bool signal -> ?enabled:bool signal ->
    ?tip:Jstr.t signal -> ?exts:string list -> El.t list signal ->
    File.t list t
  (** [files_selector] is like {!file_selector} but allows multiple
      files to be selected. *)

  (** {1:style Styling}

      The element returned by {!el} makes use of the following CSS
      classes:
      {ul
      {- [ui-button] always.}
      {- [ui-file-selector] always iff created via {!file_selector}
         or {!files_selector}}
      {- [ui-active] whenever {!active} is [true].}
      {- [ui-disabled] whenever {!enabled} is [false].}} *)
end

(** String editors.

    String editors are for editing short strings. See the
    {{!style}styling information}. *)
module Jstr_editor : sig

  (** {1:str String editors} *)

  type t
  (** The type for string editors. *)

  val v :
    ?class':Jstr.t -> ?enabled:bool signal -> ?on:'a event ->
    ?length:int signal -> Jstr.t signal -> t
  (** [v ~class' ~enabled ~on ~size s] is an editor for a string:
      {ul
      {- [s] is the string value to edit.}
      {- [length] is the length of the editor in number of characters
         (defaults to [S.const 20]).}
      {- [on] can be used to put the string editor on focus and in
         editing mode.}
      {- [enabled] indicates if the editor can be interacted with
         defaults to {!S.Bool.true'}.}
      {- [class'] is added to the underlying element's classes.}} *)

  val action : t -> Jstr.t event
  (** [action e] occurs with a new string when an edition was validated. *)

  val enabled : t -> bool signal
  (** [enabled e] is [true] iff the editor is enabled. *)

  val editing : t -> bool signal
  (** [editing e] is [true] whenever the string is being edited. *)

  val el : t -> El.t
  (** [el b] is [b]'s DOM element. *)

  (** {1:style Styling}

      The element returned by {!el} makes use of the following CSS
      classes:
      {ul
      {- [ui-str-editor] always.}
      {- [ui-editing] whenever {!editing} is [true].}
      {- [ui-disabled] whenever {!enabled} is [false].}} *)
end

(** Value selectors

    Value selector allow to select a value among a finite number
    of choices. *)
module Value_selector : sig

  (** Menu selector

      The value is selected in a list of elements
      via a drop down menu. See the {{!style}styling information}. *)
  module Menu : sig

    (** {1:selector Selectors} *)

    type 'a t
    (** The type for menu selector of values of type ['a]. *)

    val v :
      ?class':Jstr.t -> ?enabled:bool signal -> ('a -> Jstr.t) ->
      'a list signal -> 'a signal -> 'a t
    (** [v ~class' ~enabled label choices sel] is a menu for
        selecting a value. [S.eq sel] is used to test values
        for equality in the list of [choices].
        {ul
        {- [label] is used to label the values to select}
        {- [choices] are the values among which to select}
        {- [sel] is the value shown as selected it must be included in
         [choices]}
        {- [enabled] indicates if the selector can be
         interacted with. Defaults to {!S.Bool.true'}}
        {- [class'] is added to the underlying element's classes.}} *)

    val action : 'a t -> 'a event
    (** [action s] occurs whenever a new value is selected. *)

    val enabled : 'a t -> bool signal
    (** [enabled s] is [true] iff the selector is enabled. *)

    val el : 'a t -> El.t
    (** [el s] is [s]'s DOM element. *)

    (** {1:style Styling}

        The element returned by {!el} makes use of the following CSS
        classes:
        {ul
        {- [ui-menu-selector] always.}
        {- [ui-disabled] whenever {!enabled} is [false].}} *)
  end

  (** Button selectors.

      The value is selected by clicking in a list of buttons.
      See the {{!style}styling information}. *)
  module Button : sig

    (** {1:selector Selector} *)

    val v :
      ?class':Jstr.t -> ?enabled:bool signal ->
      ?button_class:('a -> Jstr.t) -> ?button_tip:('a -> Jstr.t signal) ->
      ?xdir_align:Group.align -> ?dir_align:Group.align -> dir:Group.dir ->
      ('a -> El.t list signal) -> 'a list signal -> 'a option signal ->
      'a Group.t
      (** [v ~class' ~enaled ~eq label choices sel] is list of buttons for
          selecting a value. [S.eq sel] is used to test values
          for equality in the list of [choices].
          {ul
          {- [label] is used to label the choice buttons.}
          {- [choices] are the values among which to select}
          {- [sel] is the value shown as selected, if any. It must be
          included in [choices]}
          {- [button_class] is a class for choice buttons.}
          {- [enabled] indicates if the selector can be
          interacted with. Defaults to {!S.Bool.true'}}
          {- [class'] is added to the underlying element's classes.}}

          The {!Group.action} of the result occurs whenever a new
          selection occurs. *)

    (** {1:style Styling}

        The returned group and buttons makes use of the following CSS
        classes (the {{!Group.style}group styling} also applies):
        {ul
        {- [ui-button-selector] always on the group}
        {- [ui-selected] on the button currently selected}
        {- [ui-disabled] on the group and buttons whenever
         {!enabled} is [false].}} *)
  end
end

(** Floating point value selector.

    The value is selected by a slider.
    See the {{!style}styling information}. *)
module Float_selector : sig

  (** {1:selector Selector} *)

  type t
  val v :
    ?class':Jstr.t ->
    ?enabled:bool signal -> ?min:float signal -> ?max:float signal ->
    ?step:float option signal -> float signal -> t

  val action : t -> float event
  (** [action b] is an event that occurs when a new float is selected. *)

  val enabled : t -> bool signal
  (** [enabled b] is a signal that is [true] iff the selector is enabled. *)

  val el : t -> El.t

  (** {1:style Styling} *)
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
