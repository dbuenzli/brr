brr — Browser programming toolkit for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Brr is a toolkit for programming browsers in OCaml with the
[`js_of_ocaml`][jsoo] compiler. It provides:

* Interfaces to a selection of browser APIs.
* Note based reactive support (optional and experimental).
* An OCaml console developer tool for live interaction 
  with programs running in web pages.
* A JavaScript FFI for idiomatic OCaml programming.

Brr is distributed under the ISC license. It depends on [Note][note]
and on the `js_of_ocaml` compiler and runtime – but not on its
libraries or syntax extension.

[note]: https://erratique.ch/software/note
[jsoo]: https://ocsigen.org/js_of_ocaml

Homepage: https://erratique.ch/software/brr  

## Installation

Brr can be installed with `opam`:

    opam install brr

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc brr`.

[doc]: https://erratique.ch/software/brr/doc

## Sample programs

Not much for now. A few basic programs are in the [test suite][test].

An implementation of the [TodoMVC][todomvc] application with `brr.note` is 
in [todomvc.ml](test/todomvc.ml).

[todomvc]: http://todomvc.com/
