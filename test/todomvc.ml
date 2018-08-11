(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* The implementation respects the framework given at
   http://todomvc.com/. Notably the CSS file and markup
   structure. Some things could be implemented differently, organized
   in a more natural, modular and generic manner by lifting this
   restriction.

   The UX was also kept as defined by the backbone.js reference
   implementation -- but it's not always good. Here are a few things
   that could be improved:

   1. The footer should not be placed at the bottom of the todo list.
      First this means you have to scroll if you have many
      todos. Second it moves away from your pointer when you click on
      a filter which is very annoying when you want to quickly switch
      between filters.

   2. Operating on the data when filters are active is a bit
      confusing. For example adding a new todo when the filter is
      "Completed" should disable the filter otherwise the user has the
      feeling no new item is being added. Toggling completeness is
      equally confusing (though a bit of animation could do here).

   3. The "toggle all" button look and behaviour is a bit confusing. *)

open Note
open Brr

let pf = Format.fprintf

(* Model *)

module Todo : sig
  type t
  val v : str -> t
  val task : t -> str
  val done' : t -> bool
  val with_task : str -> t -> t
  val with_done : bool -> t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { task : str; done' : bool }
  let v task = { task; done' = false }
  let task t = t.task
  let done' t = t.done'
  let with_task task t = { t with task }
  let with_done done' t = { t with done' }
  let pp ppf t = pf ppf "[%c] %a" (if t.done' then 'x' else ' ') Str.pp t.task
end

module Todos : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val to_list : t -> Todo.t list
  val count : t -> int
  val add : Todo.t -> t -> t
  val rem : Todo.t -> t -> t
  val replace : Todo.t -> by:Todo.t -> t -> t
  val map : (Todo.t -> Todo.t) -> t -> t
  val fold : (Todo.t -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (Todo.t -> bool) -> t -> t
  val for_all : (Todo.t -> bool) -> t -> bool
  val exists : (Todo.t -> bool) -> t -> bool
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Todo.t list
  let empty = []
  let is_empty ts = ts = empty
  let to_list ts = ts
  let count ts = List.length ts
  let update upd t ts =
    let upd t acc = match upd t with None -> acc | Some v -> (v :: acc) in
    let rec loop acc = function
    | [] -> List.rev acc
    | t' :: ts when t == t' -> List.rev_append (upd t acc) ts
    | t' :: ts -> loop (t' :: acc) ts
    in
    loop [] ts

  let add t ts = t :: ts
  let rem = update (fun _ -> None)
  let replace t ~by = update (fun _ -> Some by) t
  let map f ts = List.(rev @@ rev_map f ts)
  let fold f ts acc = List.fold_left (fun acc t -> f t acc) acc ts
  let filter sat = List.filter sat
  let for_all sat = List.for_all sat
  let exists sat = List.exists sat
  let pp ppf ts = pf ppf "@[<v>%a@]" (Format.pp_print_list Todo.pp) ts
end

(* Model actions *)

type add_action = [ `Add_todo of str ]
type bulk_action = [ `All_done of bool | `Rem_done ]
type edit_action =
[ `Set_task of str * Todo.t
| `Set_done of bool * Todo.t
| `Rem_todo of Todo.t ]

type action = [ add_action | bulk_action | edit_action ]

let do_action : action -> Todos.t -> Todos.t = function
| `Add_todo task -> Todos.add (Todo.v task)
| `Set_task (task, todo) -> Todos.replace todo ~by:(Todo.with_task task todo)
| `Set_done (d, todo) -> Todos.replace todo ~by:(Todo.with_done d todo)
| `Rem_todo todo -> Todos.rem todo
| `All_done d -> Todos.map (Todo.with_done d)
| `Rem_done -> Todos.filter (fun t -> not (Todo.done' t))

(* Persisting *)

let () = Store.force_version "0"
let state : Todos.t Store.key = Store.key ~ns:(str "todos-brr") ()
let save_state ts = Store.add state ts
let load_state () = Store.get ~absent:Todos.empty state

(* Rendering & interaction *)

let el_def_display : El.t -> bool signal -> unit =
  (* Would maybe be better to do this via CSS classes *)
  let none = Js.string "none" and show = Js.string "" in
  let bool_to_display = function true -> show | false -> none in
  fun el bool -> El.def_style El.Style.display (S.map bool bool_to_display) el

let add_todo : unit -> [> add_action] event * [> El.t] =
fun () ->
  let p = "What needs to be done ?" in
  let atts = Att.[type' "text"; class' "new-todo"; autofocus; placeholder p] in
  let i = El.input ~atts [] in
  let return = E.filter Ev.(for_el i keydown Key.of_ev) (Key.equal `Return) in
  let input = E.map (El.rget_prop Prop.value i ~on:return) Str.trim in
  let add_todo = E.filter_map input @@ fun v -> match Str.is_empty v with
  | true -> None
  | false -> Some (`Add_todo v)
  in
  let clear = E.stamp add_todo Str.empty in
  let () = El.rset_prop Prop.value i ~on:clear in
  add_todo, i

let toggle_all : set:bool signal -> [> bulk_action ] event * [> El.t] =
fun ~set ->
  let tid = "toggle-all" in
  let i = El.input ~atts:Att.[type' "checkbox"; class' tid; id tid] [] in
  let () = El.def_prop Prop.checked set i in
  let toggle = El.rget_prop Prop.checked i ~on:Ev.(for_el i click unit) in
  let toggle = E.map toggle @@ fun checked -> `All_done checked in
  let label = El.(label ~atts:Att.[for' tid] [txt "Mark all as complete"]) in
  toggle, El.div [i; label]

let items_left : count:int signal -> [> El.t] =
fun ~count ->
  let count_msg = function
  | 0 -> El.txt "0 items left"
  | 1 -> El.txt "1 item left"
  | n -> El.txtf "%d items left" n
  in
  let span = El.span ~atts:Att.[class' "todo-count"] [] in
  let () = El.def_children span (S.map count @@ fun c -> [count_msg c]) in
  span

type filter = [ `All | `Todo | `Done ]
let filters : unit -> filter signal * [> El.t] =
fun () ->
  let parse_frag frag = match Str.to_string frag with
  | "#/active" -> `Todo | "#/completed" -> `Done | v -> `All
  in
  let init_filter = parse_frag (Loc.fragment ()) in
  let filter_li frag name =
    let a = El.(a ~atts:Att.[v Name.href frag] [txt name]) in
    let sel = parse_frag frag = init_filter in
    let selected = S.hold sel (E.map Loc.hashchange (Str.equal frag)) in
    let () = El.def_class (str "selected") selected a in
    El.li [a]
  in
  let all = filter_li (str "#/") "All" in
  let todo = filter_li (str "#/active") "Active" in
  let done' = filter_li (str "#/completed") "Completed" in
  let filter = S.hold init_filter (E.map Loc.hashchange parse_frag) in
  filter, El.ul ~atts:Att.[class' "filters"] [all; todo; done']

let str_editor : str -> on:'a event -> bool event * str event * [> El.t] =
fun s ~on ->
  let ed = El.input ~atts:Att.[class' "edit"; v Name.value s] [] in
  let keys = Ev.(for_el ed keydown Key.of_ev) in
  let edited = E.filter keys (Key.equal `Return) in
  let undo = E.filter keys (Key.equal `Escape) in
  let start_edit = E.stamp on true in
  let stop_edit = E.stamp (E.select [edited; undo]) false in
  let editing = E.select [start_edit; stop_edit] in
  let str = El.rget_prop Prop.value ed ~on:edited  in
  let () = El.rset_prop Prop.value ~on:(E.map undo @@ fun _ -> s) ed in
  let () = El.rset_focus ~on:start_edit ed in
  let () = El.(call (fun _ e -> select_txt e) ~on:start_edit ed) in
  editing, str, ed

let bool_editor : bool -> bool event * [> El.t ] =
fun bool ->
  let atts = Att.(add_if bool checked [type' "checkbox"; class' "toggle"]) in
  let el = El.input ~atts [] in
  let toggle = El.rget_prop Prop.checked el ~on:Ev.(for_el el click unit) in
  toggle, el

let todo_item : Todo.t -> [> edit_action ] event * [> El.t] =
fun todo ->
  let done' = Todo.done' todo in
  let task = Todo.task todo in
  let set_done, done_editor = bool_editor done' in
  let set_done = E.map set_done @@ fun d -> `Set_done (d, todo) in
  let rem_but = El.button ~atts:Att.[class' "destroy"] [] in
  let rem = Ev.(for_el rem_but click @@ stamp (`Rem_todo todo)) in
  let label = El.label [`Txt task] in
  let editing, edited, ed =
    str_editor task ~on:Ev.(for_el label dblclick unit)
  in
  let edit = E.filter_map edited @@ fun v ->
    let v = Str.trim v in
    if Str.is_empty v then Some (`Rem_todo todo) else
    if not (Str.equal v task) then Some (`Set_task (v, todo)) else None
  in
  let div = El.div ~atts:Att.[class' "view"] [done_editor; label; rem_but] in
  let li = El.li ~atts:Att.(add_if done' (class' "completed") []) [div; ed] in
  let () = El.rset_class (str "editing") editing li in
  E.select [edit; rem; set_done], li

let todo_list :
  Todos.t signal -> filter:filter signal -> [> edit_action ] event * [> El.t] =
fun ts ~filter ->
  let add filter t (es, is as acc) = match filter with
  | `Todo when Todo.done' t -> acc
  | `Done when not (Todo.done' t) -> acc
  | _ -> let e, i = todo_item t in (e :: es, i :: is)
  in
  let add_todos ts filter = Todos.fold (add filter) ts ([], []) in
  let items = S.l2 ~eq:( == ) add_todos ts filter in
  let act = E.swap @@ S.map ~eq:( == ) items @@ fun (evs, _) -> E.select evs in
  let items = S.map items snd in
  let ul = El.ul ~atts:Att.[class' "todo-list"] [] in
  let () = El.def_children ul items in
  act, ul

let header () =
  let add, field = add_todo () in
  add, El.(header ~atts:Att.[class' "header"] [h1 [txt "todos"]; field])

let footer ~todos =
  let is_todo t = not (Todo.done' t) in
  let has_done = S.map todos (Todos.exists Todo.done') in
  let todo_left ts = List.(length @@ filter is_todo (Todos.to_list ts)) in
  let left_el = items_left ~count:(S.map todos todo_left) in
  let filter, fs_el = filters () in
  let rem_done, rem_el =
    let atts = Att.[class' "clear-completed"] in
    let b = El.button ~atts [El.txt "Clear completed"] in
    let () = el_def_display b has_done in
    let rem_done = Ev.(for_el b click @@ stamp `Rem_done) in
    rem_done, b
  in
  let foot = El.footer ~atts:Att.[class' "footer"] [left_el; fs_el; rem_el] in
  let display ts = not @@ Todos.is_empty ts in
  let () = el_def_display foot (S.map todos display) in
  filter, rem_done, foot

let main ~add_todo ~rem_done ~todos ~filter =
  let toggle_set = S.map todos @@ fun ts ->
    Todos.(not (is_empty ts) && for_all Todo.done' ts)
  in
  let toggle_all, toggle_el = toggle_all toggle_set in
  let edit, items = todo_list todos ~filter in
  let sec = El.section ~atts:Att.[class' "main"] [toggle_el; items] in
  let display ts = not @@ Todos.is_empty ts in
  let () = el_def_display sec (S.map todos display) in
  E.select [add_todo; rem_done; edit; toggle_all], sec

let ui : todos:Todos.t -> (Todos.t signal * El.child list) =
fun ~todos ->
  let def todos =
    let add_todo, header = header () in
    let filter, rem_done, footer = footer ~todos in
    let action, main = main ~add_todo ~rem_done ~todos ~filter in
    let do_action = E.map action do_action in
    let todos' = S.accum (S.value todos) do_action in
    todos', (todos', [header; main; footer])
  in
  S.fix todos def

let main () =
  let id = str "app" in
  match El.find ~id with
  | None -> Log.err (fun m -> m  "No element with id '%a' found" Str.pp id)
  | Some el ->
      let todos, children = ui ~todos:(load_state ()) in
      Logr.(hold @@ S.log todos save_state);
      El.set_children el children

let () = App.run ~name:"todomvc" main

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
