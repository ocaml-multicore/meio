meio
----

*Status: Experimental*

Meio is a CLI tool for monitoring programs using [Eio][]. It allows you to see, in real-time, the running and idle fibers of your program along with the structure of the fiber tree. Fibers are also labelled with where they were spawned from and each individual fiber carries extra information about how busy it is, how often it is entered and the debug logs that have been run from the fiber. Here's what Meio looks like:

![Meio on asciicast](./.screencast/example.gif)

### Building meio

Meio uses custom events which will be available in OCaml 5.1. This means currently you must use a few forked libraries.

To build Meio locally:

```
$ opam switch create . 5.1.1 --no-install
$ opam install --deps-only .
$ dune build
```

You can install meio with `opam install ./meio.opam` and then run one of the example programs from Meio.

```
$ meio _build/default/example/burn.exe
```

[Eio]: https://github.com/ocaml-multicore/eio
