open B0_kit.V000
open B00_std

(* OCaml library names *)

let js_of_ocaml = B0_ocaml.libname "js_of_ocaml"
let js_of_ocaml_compiler_runtime =
  B0_ocaml.libname "js_of_ocaml-compiler.runtime"

let js_of_ocaml_toplevel = B0_ocaml.libname "js_of_ocaml-toplevel"
let note = B0_ocaml.libname "note"

let brr = B0_ocaml.libname "brr"
let brr_note = B0_ocaml.libname "brr.note"
let brr_ocaml_poke = B0_ocaml.libname "brr.ocaml_poke"
let brr_ocaml_poke_ui = B0_ocaml.libname "brr.ocaml_poke_ui"
let brr_poke = B0_ocaml.libname "brr.poke"
let brr_poked = B0_ocaml.libname "brr.poked"

(* Units *)

let brr_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [js_of_ocaml_compiler_runtime] in
  B0_ocaml.lib brr ~doc:"Brr JavaScript FFI and browser API" ~srcs ~requires

let brr_note_lib =
  let srcs = Fpath.[ `Dir (v "src/note") ] in
  let requires = [brr; note] in
  B0_ocaml.lib brr_note ~doc:"Brr Note support" ~srcs ~requires

let brr_ocaml_poke_lib =
  let srcs = Fpath.[ `Dir (v "src/ocaml_poke") ] in
  let requires = [brr] in
  let doc = "OCaml poke objects interaction" in
  B0_ocaml.lib brr_ocaml_poke ~doc ~srcs ~requires

let brr_ocaml_poke_ui_lib =
  let srcs = Fpath.[ `Dir (v "src/ocaml_poke_ui") ] in
  let requires = [brr; brr_ocaml_poke] in
  let doc = "OCaml poke user interface (toplevel)" in
  B0_ocaml.lib brr_ocaml_poke_ui ~doc ~srcs ~requires

let brr_poke_lib =
  let srcs = Fpath.[ `Dir (v "src/poke") ] in
  let requires = [js_of_ocaml; js_of_ocaml_toplevel; brr] in
  let doc = "Poke explicitely" in
  B0_ocaml.lib brr_poke ~doc ~srcs ~requires

let brr_poked_lib =
  let srcs = Fpath.[ `Dir (v "src/poked") ] in
  let requires = [brr_poke] in
  let doc = "Poke by side effect" in
  B0_ocaml.lib brr_poked ~doc ~srcs ~requires

(* Web extension *)

let console =
  let srcs = Fpath.[ `Dir (v "src/console");
               `X (v "src/console/ocaml_console.js"); (* GNGNGNGN *)
               `X (v "src/console/devtools.js");
               `X (v "src/console/highlight.pack.js");
               (* FIXME we want something like ext_js *) ]
  in
  let requires = [brr; brr_ocaml_poke; brr_ocaml_poke_ui] in
  let comp_mode = `Whole and source_map = Some `Inline in
  let comp = Cmd.(atom "--pretty") in
  let meta = B0_jsoo.meta ~requires ~comp ~comp_mode ~source_map () in
  let doc = "Browser developer tool OCaml console" in
  B0_jsoo.web "ocaml_console" ~doc ~srcs ~meta

let poke =
  let srcs = Fpath.[ `File (v "test/poke.ml"); `File (v "test/base.css") ] in
  let requires = [brr; brr_note; brr_poked] in
  let meta = B0_jsoo.meta ~requires ~toplevel:true () in
  let doc = "OCaml console test" in
  B0_jsoo.web "poke" ~doc ~srcs ~meta

let top =
  let srcs = Fpath.[ `File (v "test/top.ml");
(* FIXME js_of_ocaml chokes `File "src/console/highlight.pack.js"; *)
                     `File (v "src/console/ocaml_console.css") ] in
  let requires =
    [ js_of_ocaml; js_of_ocaml_toplevel; brr; brr_note; brr_ocaml_poke_ui;
      brr_poke; brr_ocaml_poke]
  in
  let comp_mode = `Whole in
  let meta = B0_jsoo.meta ~requires ~comp_mode ~toplevel:true () in
  let doc = "In page toplevel test" in
  B0_jsoo.web "top" ~doc ~srcs ~meta

(* Tests and samples *)

let test_assets = Fpath.[ `File (v "test/base.css") ]

let test ?(requires = [brr]) n ~doc =
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" n)) :: test_assets in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.web n ~doc ~srcs ~meta

let test_module ?doc top m requires  =
  let test = Fmt.str "test_%s" (String.Ascii.uncapitalize m) in
  let doc = Fmt.str "Test %s.%s module" top m in
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" test)) :: test_assets in
  let comp = Cmd.(atom "--pretty") in
  let meta = B0_jsoo.meta ~requires ~comp () in
  B0_jsoo.web test ~doc ~srcs ~meta

let hello = test "test_hello" ~doc:"Brr console hello size"

let test_mutobs =
  let doc = "Test use of MutationObservers by Brr_note" in
  test "test_mutobs" ~doc

let test_leak =
  let requires = [note; brr; brr_note] in
  test "test_leak" ~requires ~doc:"Tests reactive DOM gc strategy"

let test_base64 = test_module "Brr" "Base64" [brr]
let test_c2d = test_module "Brr_canvas" "C2d" [brr]
let test_clipboard = test_module "Brr_io" "Clipboard" [brr]
let test_console = test_module "Brr" "Console" [brr]
let test_file = test_module "Brr" "File" [brr]
let test_geo = test_module "Brr_io" "Geolocation" [brr]
let test_gl = test_module "Brr_canvas" "Gl" [brr]
let test_history = test_module "Brr" "History" [brr]
let test_key = test_module "Brr_note_kit" "Key" [note; brr; brr_note]
let test_media = test_module "Brr_io" "Media" [brr]
let test_mouse = test_module "Brr_note_kit" "Mouse" [note; brr; brr_note]
let test_notif = test_module "Brr_io" "Notification" [brr]
let test_webaudio = test_module "Brr_webaudio" "Audio" [brr]
let test_webcryto = test_module "Brr_webcrypto" "Crypto" [brr]
let test_worker = test_module "Brr" "Worker" [brr]

let min =
  let srcs = Fpath.[ `File (v "test/min.ml"); `File (v "test/min.html") ] in
  let requires = [brr] in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.web "min" ~doc:"Brr minimal web page" ~srcs ~meta

let nop =
  let srcs = Fpath.[ `File (v "test/nop.ml") ] in
  let meta = B0_jsoo.meta ~requires:[] () in
  B0_jsoo.web "nop" ~doc:"js_of_ocaml nop web page" ~srcs ~meta

let todomvc =
  let srcs = Fpath.[ `File (v "test/todomvc.ml");
                     `File (v "test/todomvc.html"); ]
  in
  let requires = [note; brr; brr_note;] in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.web "todomvc" ~doc:"TodoMVC app" ~srcs ~meta

(* Packs *)

let test_pack = (* FIXME add stuff in b0 *)
  let us = [ test_console ] in
  let meta = B0_meta.(empty |> tag test) in
  B0_pack.v ~locked:false "test" ~doc:"Brr test suite" ~meta us

let jsoo_toplevels =
  (* FIXME this is wrong and make that nice to write *)
  let tops = B0_unit.has_meta B0_jsoo.toplevel in
  let us = List.filter tops (B0_unit.list ()) in
  let doc = "Units with toplevel (slow to build)" in
  B0_pack.v ~locked:false "tops" ~doc us

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The brr programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/brr"
    |> add online_doc "https://erratique.ch/software/brr/doc/"
    |> add licenses ["ISC"; "BSD-3-Clause"]
    |> add repo "git+https://erratique.ch/repos/brr.git"
    |> add issues "https://github.com/dbuenzli/brr/issues"
    |> add description_tags
      [ "reactive"; "declarative"; "frp"; "front-end"; "browser";
        "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "js_of_ocaml-compiler", {|>= "4.0.0"|};
        "js_of_ocaml-toplevel", {|>= "4.0.0"|};
        "note", "";
      ]
  in
  B0_pack.v "default" ~doc:"brr package" ~meta ~locked:true @@
  B0_unit.list ()

(* Cmdlets *)

let update_console =
  B0_cmdlet.v ~doc:"Develop console" "dev-console" @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  (* FIXME so much path boilerplate. Summon units/packs to be build
     and get info directly. *)
  let b0_dir = B0_cmdlet.Env.b0_dir env in
  let variant = "user" in
  let build_dir = B0_dir.build_dir ~b0_dir ~variant in
  let unit_dir = B0_dir.unit_build_dir ~build_dir ~name:"ocaml_console" in
  let src = Fpath.(unit_dir / "ocaml_console.js") in
  let dst = Fpath.(v "src" / "console" / "ocaml_console.js") in
  let dst = B0_cmdlet.in_scope_dir env dst in
  let cmd = Cmd.(atom "b0" % "-u" % "ocaml_console" % "-u" % "poke") in
  Result.bind (Os.Cmd.run cmd) @@ fun () ->
  Os.File.copy ~force:true ~make_path:false ~src dst
