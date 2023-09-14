meio
----

*Status: Experimental*

Meio is a CLI tool for monitoring programs asynchronous programs like those that use [Eio][]. It allows you to see, in real-time, the running and idle fibers of your program along with the structure of the fiber tree. Fibers are also labelled with where they were spawned from and each individual fiber carries extra information about how busy it is, how often it is entered and the debug logs that have been run from the fiber. Here's what Meio looks like:

![Meio on asciicast](./.screencast/example.gif)

### Building meio

Meio uses custom events which is only available in OCaml 5.1 and beyond.

```
$ opam update
$ opam switch create 5.1.0
$ opam install --deps-only .
$ dune build
```

You can install meio with `dune install` and then run one of the example programs from Meio.

```
$ meio _build/default/example/burn.exe
```

[Eio]: https://github.com/ocaml-multicore/eio
