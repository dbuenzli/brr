(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note

module Key = struct
  type code = int
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

  (* For browser keyboard handling see http://unixpapa.com/js/key.html *)

  let of_keycode kc = match kc with
  | c when 48 <= c && c <= 57 -> `Ascii (Char.chr c)
  | c when 65 <= c && c <= 90 -> `Ascii (Char.chr (c + 32) (* to lower *))
  | c when 96 <= c && c <= 105 -> `Ascii (Char.chr (c - 96 + 48))
  | c when 112 <= c && c <= 135 -> `Func (c - 111)
  | 8 -> `Backspace
  | 9 -> `Tab
  | 13 -> `Return
  | 16 -> `Shift `Left
  | 17 -> `Ctrl `Left
  | 18 -> `Alt `Left
  | 27 -> `Escape
  | 32 -> `Spacebar
  | 33 -> `Page `Up
  | 34 -> `Page `Down
  | 35 -> `End
  | 36 -> `Home
  | 37 -> `Arrow `Left
  | 38 -> `Arrow `Up
  | 39 -> `Arrow `Right
  | 40 -> `Arrow `Down
  | 45 -> `Enter
  | 91 | 224 -> `Meta `Left
  | 93 -> `Meta `Right
  | c -> `Key c

  let of_ev e = of_keycode (Jv.Int.get (Ev.to_jv e) "keyCode")
  let equal k0 k1 = k0 = k1
  let compare k0 k1 = compare k0 k1

  let dir_to_jstr = function
  | `Left -> Jstr.v "left" | `Right -> Jstr.v "right"
  | `Up -> Jstr.v "up" | `Down -> Jstr.v "down"

  let to_jstr = function
  | `Alt dir -> Jstr.(v "alt_" + dir_to_jstr dir)
  | `Arrow dir -> Jstr.(v "arrow_" + dir_to_jstr dir)
  | `Ascii c -> Jstr.(v "key_" + of_char c) (* FIXME escape *)
  | `Backspace -> Jstr.v "backspace"
  | `Ctrl dir -> Jstr.(v "ctrl_" + dir_to_jstr dir)
  | `End -> Jstr.v "end"
  | `Enter -> Jstr.v "enter"
  | `Escape -> Jstr.v "escape"
  | `Func n -> Jstr.(v "F" + of_int n)
  | `Home -> Jstr.v "home"
  | `Insert -> Jstr.v "insert"
  | `Key c -> Jstr.(v "key_" + of_int c)
  | `Meta dir -> Jstr.(v "meta_" + dir_to_jstr dir)
  | `Page dir -> Jstr.(v "page_" + dir_to_jstr dir)
  | `Return -> Jstr.v "return"
  | `Shift dir -> Jstr.(v "shift_" + dir_to_jstr dir)
  | `Spacebar -> Jstr.v "spacebar"
  | `Tab -> Jstr.v "tab"

  (* FIXME remove use of Hashtbl, do it in Js. *)

  type events =
    { any_down : t event; send_any_down : t E.send;
      any_up : t event; send_any_up : t E.send;
      mutable down_count : int;
      any_holds : bool signal; set_any_holds : bool S.set;
      down_event : (t, unit event * unit E.send) Hashtbl.t ;
      up_event : (t, unit event * unit E.send) Hashtbl.t;
      holds : (t, bool signal * bool S.set) Hashtbl.t;
      alt : bool signal; ctrl : bool signal; meta : bool signal;
      shift : bool signal; }

  let def_event event k = try fst (Hashtbl.find event k) with
  | Not_found -> let d = E.create () in Hashtbl.add event k d; fst d

  let send_event ?step event k = try snd (Hashtbl.find event k) ?step () with
  | Not_found -> ()

  let def_holds holds k = try fst (Hashtbl.find holds k) with
  | Not_found -> let d = S.create false in Hashtbl.add holds k d; fst d

  let set_holds ?step holds k v = try snd (Hashtbl.find holds k) ?step v with
  | Not_found -> ()

  let add_modifiers holds =
     let lalt = S.create false in
     let ralt = S.create false in
     let alt = S.Bool.(fst lalt || fst ralt) in
     let lctrl = S.create false in
     let rctrl = S.create false in
     let ctrl = S.Bool.(fst lctrl || fst rctrl) in
     let lmeta = S.create false in
     let rmeta = S.create false in
     let meta = S.Bool.(fst lmeta || fst rmeta) in
     let lshift = S.create false in
     let rshift = S.create false in
     let shift = S.Bool.(fst lshift || fst rshift) in
     Hashtbl.add holds (`Alt `Left) lalt;
     Hashtbl.add holds (`Alt `Right) ralt;
     Hashtbl.add holds (`Ctrl `Left) lctrl;
     Hashtbl.add holds (`Ctrl `Right) rctrl;
     Hashtbl.add holds (`Meta `Left) lmeta;
     Hashtbl.add holds (`Meta `Right) rmeta;
     Hashtbl.add holds (`Shift `Left) lshift;
     Hashtbl.add holds (`Shift `Right) rshift;
     alt, ctrl, meta, shift

  let handle_down evs ~step k =
    evs.down_count <- evs.down_count + 1 ;
    evs.send_any_down ~step k;
    evs.set_any_holds ~step true;
    send_event ~step evs.down_event k;
    set_holds ~step evs.holds k true;
    ()

  let handle_up evs ~step k =
    evs.down_count <- evs.down_count - 1;
    evs.send_any_up ~step k;
    if evs.down_count <= 0 then
      (evs.down_count <- 0; evs.set_any_holds ~step false);
    send_event ~step evs.up_event k;
    set_holds ~step evs.holds k false;
    ()

  (* Unclear how well that repeat works. Otherwise suppress
     repeats like we did in Useri. *)
  let down_cb evs e =
    if Ev.(Keyboard.repeat (as_type e)) then () else
    let step = Step.create () in
    handle_down evs ~step (of_ev e);
    Step.execute step

  let up_cb evs e =
    let step = Step.create () in
    handle_up evs ~step (of_ev e);
    Step.execute step

  let on_target ?capture ?propagate ?default t =
    let hsize = 47 in
    let any_down, send_any_down = E.create () in
    let any_up, send_any_up = E.create () in
    let any_holds, set_any_holds = S.create false in
    let down_event = Hashtbl.create hsize in
    let up_event = Hashtbl.create hsize in
    let holds = Hashtbl.create hsize in
    let alt, ctrl, meta, shift = add_modifiers holds in
    let evs = { any_down; send_any_down; any_up;
                send_any_up; down_count = 0; any_holds; set_any_holds;
                down_event; up_event; holds; alt; ctrl; meta; shift }
    in
    Evr.endless_listen ?capture ?propagate ?default t Ev.keydown (down_cb evs);
    Evr.endless_listen ?capture ?propagate ?default t Ev.keyup (up_cb evs);
    evs

  let on_el ?capture ?propagate ?default t =
    on_target ?capture ?propagate ?default (El.as_target t)

  let any_down evs = evs.any_down
  let any_up evs = evs.any_up
  let any_holds evs = evs.any_holds
  let down evs k = def_event evs.down_event k
  let up evs k = def_event evs.up_event k
  let holds evs k = def_holds evs.holds k
  let alt evs = evs.alt
  let ctrl evs = evs.ctrl
  let meta evs = evs.meta
  let shift evs = evs.shift
end

module Mouse = struct
  let warn_but () = Console.(warn [Jstr.v "unexpected e.which"])
  let pt x y = (x, y)

  type 'a events =
    { t : Ev.target;
      normalize : bool;
      pt : float -> float -> 'a;
      mutable last_pos : float * float;
      mutable unlisten : (unit -> unit) list;
      pos : 'a signal; set_pos : 'a S.set;
      dpos : 'a event; send_dpos : 'a E.send;
      mem : bool signal; set_mem : bool S.set;
      left : bool signal; set_left : bool S.set;
      left_down : 'a event; send_left_down : 'a E.send;
      left_up : 'a event; send_left_up : 'a E.send;
      mid : bool signal; set_mid : bool S.set;
      mid_down : 'a event; send_mid_down : 'a E.send;
      mid_up : 'a event; send_mid_up : 'a E.send;
      right : bool signal; set_right : bool S.set;
      right_down : 'a event; send_right_down : 'a E.send;
      right_up : 'a event; send_right_up : 'a E.send; }

  let destroy evs = List.iter (fun f -> f ()) evs.unlisten

  let event_mouse_pos pt evs e =
    let t = (Obj.magic evs.t : El.t) (* XXX *) in
    let x = (Ev.Mouse.client_x e) -. El.bound_x t in
    let y = (Ev.Mouse.client_y e) -. El.bound_y t in
    if not evs.normalize then pt x y else
    let nx = x /. (El.bound_w t) in
    let ny = 1. -. (y /. (El.bound_h t)) in
    pt nx ny

  let set_mouse_pos ~step evs e =
    let x, y as l = event_mouse_pos pt evs e in
    let epos = evs.pt x y in
    let dx = x -. fst evs.last_pos in
    let dy = y -. snd evs.last_pos in
    evs.send_dpos ~step (evs.pt dx dy);
    evs.set_pos ~step epos;
    evs.last_pos <- l;
    epos

  let move_cb evs e =
    let step = Step.create () in
    let _ = set_mouse_pos ~step evs (Ev.as_type e) in
    Step.execute step

  let mem_cb mem evs e =
    let step = Step.create () in
    let _ = set_mouse_pos ~step evs (Ev.as_type e) in
    evs.set_mem ~step mem;
    Step.execute step

  let down_cb evs e =
    let step = Step.create () in
    let epos = set_mouse_pos ~step evs (Ev.as_type e) in
    let set, send_down = match Ev.Mouse.button (Ev.as_type e) with
    | 0 -> evs.set_left, evs.send_left_down
    | 1 -> evs.set_mid, evs.send_mid_down
    | 2 -> evs.set_right, evs.send_right_down
    | _ -> warn_but(); evs.set_left, evs.send_left_down
    in
    set ~step true; send_down ~step epos;
    Step.execute step

  let up_cb evs e =
    let step = Step.create () in
    let epos = set_mouse_pos ~step evs (Ev.as_type e) in
    let set, send_up = match Ev.Mouse.button (Ev.as_type e) with
    | 0 -> evs.set_left, evs.send_left_up
    | 1 -> evs.set_mid, evs.send_mid_up
    | 2 -> evs.set_right, evs.send_right_up
    | _ -> warn_but (); evs.set_left, evs.send_left_up
    in
    set ~step false; send_up ~step epos;
    Step.execute step

  let doc_up_cb evs e =
    (* [up_cb] will not fire if the mouse is no longer in the target;
        but this destroys the semantics of [left], [mid], [right].
        A callback attached to the document handles this. *)
    if not (S.rough_value evs.mem) &&
       (S.rough_value evs.left || S.rough_value evs.mid ||
        S.rough_value evs.right)
    then up_cb evs e else ()

  let on_target ?capture ?propagate ?default ?(normalize = true) pt t =
    let pos, set_pos = S.create (pt 0. 0.) in
    let dpos, send_dpos = E.create () in
    let mem, set_mem = S.create false in
    let left, set_left = S.create false in
    let left_down, send_left_down = E.create () in
    let left_up, send_left_up = E.create () in
    let mid, set_mid = S.create false in
    let mid_down, send_mid_down = E.create () in
    let mid_up, send_mid_up = E.create () in
    let right, set_right = S.create false in
    let right_down, send_right_down = E.create () in
    let right_up, send_right_up = E.create () in
    let evs =
      { t; normalize; pt; last_pos = (0., 0.);
        unlisten = [];
        pos; set_pos;
        dpos; send_dpos;
        mem; set_mem;
        left; set_left; left_down; send_left_down; left_up; send_left_up;
        mid; set_mid; mid_down; send_mid_down; mid_up; send_mid_up;
        right; set_right; right_down; send_right_down; right_up; send_right_up}
    in
    let l = Evr.listen in
    let unlisten =
      [ l ?capture ?propagate ?default evs.t Ev.mousedown (down_cb evs);
        l ?capture ?propagate ?default evs.t Ev.mouseup (up_cb evs);
        l ?capture ?propagate ?default evs.t Ev.mousemove (move_cb evs);
        l ?capture ?propagate ?default evs.t Ev.mouseenter (mem_cb true evs);
        l ?capture ?propagate ?default evs.t Ev.mouseleave (mem_cb false evs);
        l ?capture ?propagate ?default
          (Document.as_target G.document) Ev.mouseup (doc_up_cb evs) ]
    in
    evs.unlisten <- unlisten; evs

  let on_el ?capture ?propagate ?default ?normalize pt e =
    let t = El.as_target e in
    let evs = on_target ?capture ?propagate ?default ?normalize pt t in
    Elr.on_rem (fun () -> destroy evs) e;
    evs

  let pos evs = evs.pos
  let dpos evs = evs.dpos
  let mem evs = evs.mem
  let left evs = evs.left
  let left_down evs = evs.left_down
  let left_up evs = evs.left_up
  let mid evs = evs.mid
  let mid_down evs = evs.mid_down
  let mid_up evs = evs.mid_up
  let right evs = evs.right
  let right_down evs = evs.right_down
  let right_up evs = evs.right_up

  module Cursor = struct
    type t = Jstr.t
    let url ?(x = 0) ?(y = 0) url = match x = 0 && y = 0 with
    | true -> Jstr.(v "url(" + url + v ")")
    | false -> Jstr.(v "url(" + url + v ") " + of_int x + sp + of_int y)

    let auto = Jstr.v "auto"
    let default = Jstr.v "default"
    let none = Jstr.v "none"
    let context_menu = Jstr.v "context-menu"
    let help = Jstr.v "help"
    let pointer = Jstr.v "pointer"
    let progress = Jstr.v "progress"
    let wait = Jstr.v "wait"
    let cell = Jstr.v "cell"
    let crosshair = Jstr.v "crosshair"
    let text = Jstr.v "text"
    let vertical_text = Jstr.v "vertical-text"
    let alias = Jstr.v "alias"
    let copy = Jstr.v "copy"
    let move = Jstr.v "move"
    let no_drop = Jstr.v "no-drop"
    let not_allowed = Jstr.v "not-allowed"
    let grab = Jstr.v "grab"
    let grabbing = Jstr.v "grabbing"
    let e_resize = Jstr.v "e-resize"
    let n_resize = Jstr.v "n-resize"
    let ne_resize = Jstr.v "ne-resize"
    let nw_resize = Jstr.v "nw-resize"
    let s_resize = Jstr.v "s-resize"
    let se_resize = Jstr.v "se-resize"
    let sw_resize = Jstr.v "sw-resize"
    let w_resize = Jstr.v "w-resize"
    let ew_resize = Jstr.v "ew-resize"
    let ns_resize = Jstr.v "ns-resize"
    let nesw_resize = Jstr.v "nesw-resize"
    let nwse_resize = Jstr.v "nwse-resize"
    let col_resize = Jstr.v "col-resize"
    let row_resize = Jstr.v "row-resize"
    let all_scroll = Jstr.v "all-scroll"
    let zoom_in = Jstr.v "zoom-in"
    let zoom_out = Jstr.v "zoom-out"
  end
end

module Windowr = struct
  let in_fullscreen () =
    Option.is_some (Document.fullscreen_element G.document)

  let is_fullscreen =
    let is_fullscreen, set_fullscreen = S.create (in_fullscreen ()) in
    let change _e = set_fullscreen (in_fullscreen ()) in
    Ev.listen Ev.fullscreenchange change (Document.as_target G.document);
    is_fullscreen

  let quit =
    let quit, send_quit = E.create () in
    let send_quit _e = send_quit () in
    Ev.listen Ev.unload send_quit (Document.as_target G.document);
    quit
end

module Time = struct
  type span = float

  let tick_now () = Performance.now_ms G.performance /. 1000. (* FIXME *)
  let start = tick_now ()
  let elapsed () = tick_now () -. start

  type counter = span
  let counter () = tick_now ()
  let counter_value c = tick_now () -. c

  let tick span =
    let e, send_e = E.create () in
    let c = counter () in
    let action () = send_e (counter_value c) in
    let ms = truncate @@ span *. 1000. in
    ignore (G.set_timeout action ~ms);
    e

  let delay span f = ignore (G.set_timeout f ~ms:(truncate @@ span *. 1000.))
  let to_jstr u s = match u with
  | `S -> Jstr.(of_float s + v "s")
  | `Ms -> Jstr.(of_float (s *. 1e3) + v "ms")
  | `Mus -> Jstr.(of_float (s *. 1e6) + v "μs")
end

module Human = struct
  let noticed = 0.1
  let interrupted = 1.
  let left = 10.

  let rec feel_action feel set_feel () =
    let new_feel, delay = match S.value feel with
    | `Interacting -> `Interrupted, left -. interrupted
    | `Interrupted -> `Left, 0.
    | `Left -> assert false
    in
    set_feel new_feel;
    if delay = 0. then () else
    let action = feel_action feel set_feel in
    let ms = truncate @@ delay *. 1000. in
    ignore (G.set_timeout ~ms action);
    ()

  let feel () =
    let feel, set_feel = S.create `Interacting in
    let action = feel_action feel set_feel in
    let ms = truncate @@ interrupted *. 1000. in
    ignore (G.set_timeout ~ms action);
    feel

  (* Sizes in mm. *)
  let touch_target_size = 9.
  let touch_target_size_min = 7.
  let touch_target_pad = 2.
  let average_finger_width = 11.
end

module Ui = struct
  (* CSS classes *)

  let ui_active = Jstr.v "ui-active"
  let ui_button = Jstr.v "ui-button"
  let ui_button_selector = Jstr.v "ui-button-selector"
  let ui_dir_align_center = Jstr.v "ui-dir-align-center"
  let ui_dir_align_distribute = Jstr.v "ui-dir-align-distribute"
  let ui_dir_align_end = Jstr.v "ui-dir-align-end"
  let ui_dir_align_justify = Jstr.v "ui-dir-align-justify"
  let ui_dir_align_start = Jstr.v "ui-dir-align-start"
  let ui_dir_align_stretch = Jstr.v "ui-dir-align-stretch"
  let ui_dir_h = Jstr.v "ui-dir-h"
  let ui_dir_v = Jstr.v "ui-dir-v"
  let ui_disabled = Jstr.v "ui-disabled"
  let ui_editing = Jstr.v "ui-editing"
  let ui_file_selector = Jstr.v "ui-file-selector"
  let ui_group = Jstr.v "ui-group"
  let ui_label = Jstr.v "ui-label"
  let ui_menu_selector = Jstr.v "ui-menu-selector"
  let ui_selected = Jstr.v "ui-selected"
  let ui_slider_selector = Jstr.v "ui-slider-selector"
  let ui_str_editor = Jstr.v "ui-str-editor"
  let ui_xdir_align_center = Jstr.v "ui-xdir-align-center"
  let ui_xdir_align_distribute = Jstr.v "ui-xdir-align-distribute"
  let ui_xdir_align_end = Jstr.v "ui-xdir-align-end"
  let ui_xdir_align_justify = Jstr.v "ui-xdir-align-justify"
  let ui_xdir_align_start = Jstr.v "ui-xdir-align-start"
  let ui_xdir_align_stretch = Jstr.v "ui-xdir-align-stretch"

  (* GUI elements. *)

  let disabled ~enabled =
    let is_disabled enabled = if enabled then None else Some Jstr.empty in
    S.map is_disabled enabled

  let el_def_tip ~tip el = match tip with
  | None -> ()
  | Some tip -> Elr.def_at At.Name.title (S.Option.some tip) el

  module Group = struct
    type dir = [ `H | `V ]
    type align = [ `Start | `End | `Center | `Justify | `Distribute | `Stretch ]
    let dir_cls = [ `H, ui_dir_h; `V, ui_dir_v; ]
    let align_cls =
      [ `Start, ui_dir_align_start; `End, ui_dir_align_end;
        `Center, ui_dir_align_center; `Justify, ui_dir_align_justify;
        `Distribute, ui_dir_align_distribute; `Stretch, ui_dir_align_stretch; ]

    let xalign_cls =
      [ `Start, ui_xdir_align_start; `End, ui_xdir_align_end;
        `Center, ui_xdir_align_center; `Justify, ui_xdir_align_justify;
        `Distribute, ui_xdir_align_distribute; `Stretch, ui_xdir_align_stretch; ]

    let set_class classes el v = El.set_class (List.assoc v classes) true el

    type 'a t =
      { el : El.t;
        enabled : bool signal;
        action : 'a event;
        dir : dir;
        dir_align : align;
        xdir_align : align; }

    let v
        ?class':cl ?(enabled = S.Bool.true') ?(action = E.never)
        ?(xdir_align = `Start) ?(dir_align = `Start) ~dir cs
      =
      let at = At.[if_some (Option.map class' cl); class' ui_group] in
      let el = El.div ~at [] in
      let () = Elr.def_children el cs
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) el
      and () = set_class dir_cls el dir
      and () = set_class align_cls el dir_align
      and () = set_class xalign_cls el xdir_align in
      { el; enabled; action; dir; dir_align; xdir_align }

    let dir g = g.dir
    let dir_align g = g.dir_align
    let xdir_align g = g.xdir_align
    let action g = g.action
    let enabled g = g.enabled
    let el g = g.el
    let with_action action g = { g with action }
    let hide_action g = with_action E.never g
  end

  module Label = struct
    type t = { el : El.t; enabled : bool signal }
    let v ?class':cl ?(enabled = S.Bool.true') ?tip cs =
      let at = At.[if_some (Option.map class' cl); class' ui_label] in
      let el = El.div ~at [] in
      let () = Elr.def_children el cs
      and () = el_def_tip ~tip el
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) el in
      {el; enabled}

    let el l = l.el
    let enabled l = l.enabled
  end

  module Button = struct
    type 'a t =
      { el : El.t;
        action : 'a event;
        active : bool signal;
        enabled : bool signal; }

    let button_str = Jstr.v "button"
    let at_base cl =
      At.[if_some (Option.map class' cl); type' button_str; class' ui_button]

    let v
        ?class':cl ?(active = S.Bool.false') ?(enabled = S.Bool.true') ?tip cs v
      =
      let el = El.button ~at:(at_base cl) [] in
      let action = Evr.on_el Ev.click (Evr.stamp v) el in
      let () = Elr.def_children el cs
      and () = el_def_tip ~tip el
      and () = Elr.def_at At.Name.disabled (disabled ~enabled) el
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) el
      and () = Elr.def_class ui_active active el in
      (* FIXME [active] only has client defined activity *)
      { el; action; active; enabled }

    let action b = b.action
    let enabled b = b.enabled
    let active b = b.active
    let el b = b.el

    (* Special buttons *)

    let file_str = Jstr.v "file"
    let accept_str = Jstr.v "accept"
    let multiple_str = Jstr.v "multiple"
    let _file_selector
        ~multiple get ?class':cl ?(active = S.Bool.false')
        ?(enabled = S.Bool.true') ?tip ?(exts = []) cs
      =
      (* File input elements can't be customized... hence we use a button that
         forwards its clicks to the input element. *)
      let input =
        let at = match exts with
        | [] -> []
        | exts -> [ At.v accept_str (Jstr.v (String.concat "," exts)) ]
        in
        let at = At.if' multiple (At.v multiple_str Jstr.empty) :: at in
        let at = At.type' file_str :: at in
        El.input ~at ()
      in
      let el = El.button ~at:At.(class' ui_file_selector :: at_base cl) [] in
      let () = El.set_inline_style El.Style.display (Jstr.v "none") input
      and () =
        let forward e =
          (* If the same file gets selected onchange/oninput events do not refire,
             reseting the value property here works around this problem. *)
          El.set_prop El.Prop.value Jstr.empty input;
          El.click input
        in
        Ev.listen Ev.click forward (El.as_target el)
      and () =
        (* input at end for not applying * + el CSS rules, this will still disturb
           last-child and el + * though *)
        Elr.def_children el (S.map ~eq:(==) (fun cs -> cs @ [input]) cs)
      and () = el_def_tip ~tip el
      and () = Elr.def_at At.Name.disabled (disabled ~enabled) el
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) el
      and () = Elr.def_class ui_active active el
      and action = Evr.on_el Ev.change (get input) input in
      { el; enabled; action; active }

    let file_selector =
      _file_selector ~multiple:false (fun i _ -> List.hd (El.Input.files i))

    let files_selector =
      _file_selector ~multiple:true (fun i _ -> El.Input.files i)
  end

  module Jstr_editor = struct
    let text_str = Jstr.v "text"
    type t =
      { el : El.t;
        enabled : bool signal;
        editing : bool signal;
        action : Jstr.t event }

    let att_size = Jstr.v "size" (* XXX add to brr ? *)

    let v ?class':cl ?(enabled = S.Bool.true') ?on:(edit = E.never) ?length str =
      let span = El.span [] in
      let editor = El.input ~at:At.[type' text_str] () in
      let div =
        let at = At.[if_some (Option.map class' cl); class' ui_str_editor] in
        El.div ~at [span; editor]
      in
      let edit = E.select [E.stamp edit (); Evr.on_el Ev.click Evr.unit div] in
      let edit = S.sample_filter enabled ~on:edit @@ fun enabled _ ->
        if enabled then Some () else None
      in
      let keys = Evr.on_el Ev.keydown Key.of_ev editor in
      let escape_key = E.stamp (E.filter (Key.equal `Escape) keys) false in
      let return_key = E.stamp (E.filter (Key.equal `Return) keys) true in
      let start_focus = Evr.on_el Ev.focus (Evr.stamp true) editor  in
      let stop_focus = Evr.on_el Ev.blur (Evr.stamp false) editor in
      let focus =
        S.hold (El.has_focus editor) @@ E.select [start_focus;stop_focus] in
      let valid = S.hold true @@ E.select [start_focus; escape_key] in
      let start = E.stamp edit true in
      let key_stop = E.stamp (E.select [escape_key; return_key]) false in
      let stop = E.stamp (E.select [key_stop; stop_focus]) false in
      let editing = S.hold false (* FIXME *) (E.select [start; stop]) in
      let action = S.sample_filter valid ~on:stop_focus @@ fun valid _ ->
        if valid then Some (El.prop El.Prop.value editor) else None
      in
      let () = Elr.def_children span (S.map (fun s -> [El.txt s]) str)
      and () = (* FIXME the two following calls are racy. *)
        Elr.call (fun _ e -> El.select_text e) ~on:start editor
      and () = Elr.set_prop El.Prop.value ~on:(S.snapshot str ~on:edit) editor
      and () = Elr.def_has_focus focus editor
      and () = Elr.def_at At.Name.disabled (disabled ~enabled) editor
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) div
      and () = Elr.def_class ui_editing editing div
      and () = match length with
      | None -> () (* FIXME make that autogrowable. *)
      | Some l ->
          let size = S.map (fun l -> Some (Jstr.of_int l)) l in
          Elr.def_at att_size size editor
      in
      { el = div; enabled; editing; action }

    let action e = e.action
    let enabled e = e.enabled
    let editing e = e.editing
    let el e = e.el
  end

  module Value_selector = struct
    module Menu = struct
      type 'a t =
        { el : El.t;
          enabled : bool signal;
          action : 'a event }

      let v ?class':cl ?(enabled = S.Bool.true') label choices sel =
        let select =
          let at = At.[class' ui_menu_selector; class' ui_button] in
          let at = At.(if_some (Option.map class' cl)) :: at in
          El.select ~at []
        in
        let sel_idx_change =
          let extract_value e _ = Jstr.to_int @@ El.prop El.Prop.value e in
          E.Option.on_some @@
          Evr.(on_el Ev.change (extract_value select) select)
        in
        let sel_index =
          let find_sel_index eq selected choices =
            let rec loop i selected = function
            | c :: _ when eq c selected -> Jstr.of_int i
            | _ :: cs -> loop (i + 1) selected cs
            | [] -> Jstr.empty
            in
            loop 0 selected choices
          in
          S.l2 (find_sel_index (S.eq sel)) sel choices
        in
        let action = S.sample choices ~on:sel_idx_change List.nth in
        let opt i v =
          El.option ~at:At.[value (Jstr.of_int i)] [El.txt (label v)]
        in
        let opts = S.map (List.mapi opt) choices in
        let set_children opts sel_index =
          (* On children changes can't use El.def_children and El.def_prop
             it's racy. *)
          El.set_children select opts;
          El.set_prop El.Prop.value sel_index select
        in
        let set_children =
          Logr.(const set_children $ S.obs opts $ S.obs sel_index)
        in
        let () = Elr.hold_logr select (Logr.create set_children)
        and () = Elr.def_at At.Name.disabled (disabled ~enabled) select
        and () = Elr.def_class ui_disabled (S.Bool.not enabled) select
        and () = Elr.def_prop El.Prop.value sel_index select in
        { el = select; enabled; action }

      let action e = e.action
      let enabled e = e.enabled
      let el e = e.el
    end

    module Button = struct
      type 'a t = 'a Group.t
      let v
          ?class' ?(enabled = S.Bool.true') ?button_class ?button_tip
          ?xdir_align ?dir_align ~dir label choices sel
        =
        let but v =
          let class' = match button_class with
          | Some f -> Some (f v)| None -> None
          in
          let tip = match button_tip with Some f -> Some (f v) | None -> None in
          let label = label v in
          Button.v ?class' ?tip ~enabled label v
        in
        let buts = S.map ~eq:( == ) (List.map but) choices in
        let els = S.map ~eq:( == ) (List.map Button.el) buts in
        let action =
          let select buts = E.select (List.map Button.action buts) in
          E.swap @@ S.map ~eq:( == ) select buts
        in
        let sel_obs =
          let find_sel_but eq sel choices buts = match sel with
          | None ->
              let deselect b = El.set_class ui_selected false (Button.el b) in
              List.iter deselect buts
          | Some sel ->
              let rec loop sel choices buts = match choices, buts with
              | c :: cs, b :: bs when eq (Some c) (Some sel) ->
                  El.set_class ui_selected true (Button.el b);
                  loop sel cs bs
              | _ :: cs, b :: bs ->
                  El.set_class ui_selected false (Button.el b);
                  loop sel cs bs
              | [], [] -> ()
              | _, _ -> assert false
              in
              loop sel choices buts
          in
          Logr.(const (find_sel_but (S.eq sel)) $ S.obs sel $ S.obs choices $
                S.obs buts)
        in
        let g = Group.v ?class' ~action ?xdir_align ?dir_align ~dir els in
        let () = El.set_class ui_button_selector true (Group.el g) in
        let () = Elr.hold_logr (Group.el g) (Logr.create sel_obs) in
        g
    end
  end

  module Float_selector = struct
    type t =
      { el : El.t;
        enabled : bool signal;
        action : float event; }

    let range_str = Jstr.v "range"
    let min_str = Jstr.v "min"
    let max_str = Jstr.v "max"
    let step_str = Jstr.v "step"

    let v
        ?class' ?(enabled = S.Bool.true') ?(min = S.const 0.)
        ?(max = S.const 1.) ?(step = S.const None) v
      =
      let v = S.map (fun v -> Jstr.of_float v) v in
      let at = At.[type' range_str; class' ui_slider_selector; tabindex (-1)] in
      let el = El.input ~at () in
      let extract_value e _ = match El.prop El.Prop.value e with
      | s when Jstr.is_empty s -> None
      | s -> Some (float_of_string (Jstr.to_string s))
      in
      let action =
        E.Option.on_some @@ Evr.on_el Ev.input (extract_value el) el
      in
      let min_att = S.map (fun v -> Some (Jstr.of_float v)) min in
      let max_att = S.map (fun v -> Some (Jstr.of_float v)) max in
      let step_att = step |> S.map @@ function
        | None -> Some (Jstr.v "any")
        | Some f -> Some (Jstr.v @@ string_of_float f)
      in
      let () = Elr.def_at min_str min_att el
      and () = Elr.def_at max_str max_att el
      and () = Elr.def_at step_str step_att el
      and () = Elr.def_at At.Name.disabled (disabled ~enabled) el
      and () = Elr.def_class ui_disabled (S.Bool.not enabled) el
      and () = Elr.def_prop El.Prop.value v el
      and () =
        (* XXX isn't there a better way tabindex (-1) doesn't work
           also this is something that should be handled in the UI framework *)
        let unset_focus _ = El.set_has_focus false el in
        Ev.listen Ev.focus unset_focus (El.as_target el)
      in
      { el; action; enabled }

    let action r = r.action
    let enabled r = r.enabled
    let el r = r.el
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
