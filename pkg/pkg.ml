#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg


let jsoo_test ?cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ?cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ?cond ~auto:false (test ^ ".html"); ]

let build =
  let cmd c os files =
    let jsoo = Cmd.(v "-plugin-tag" % "package(js_of_ocaml.ocamlbuild)") in
    OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os %% jsoo %% of_list files)
  in
  Pkg.build ~cmd ()

let () =
  Pkg.describe ~build "brr" @@ fun c ->
  Ok [ Pkg.mllib "src/brr.mllib";
       Pkg.test "test/test";
       Pkg.test ~run:false ~auto:false ("test/test.css");
       jsoo_test "test/play";
       jsoo_test "test/leak";
       jsoo_test "test/todomvc";
       jsoo_test "test/keys"; ]
