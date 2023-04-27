meio
----

*Status: Experimental*

Meio is a CLI tool for monitoring programs using [Eio][]. It allows you to see, in real-time, the running and idle fibers of your program along with the structure of the fiber tree. Fibers are also labelled with where they were spawned from and each individual fiber carries extra information about how busy it is, how often it is entered and the debug logs that have been run from the fiber. Here's what Meio looks like:

![Meio on asciicast](./.screencast/example.gif)

### Building meio

Meio uses custom events which will be available in OCaml 5.1. This means currently you must use an unreleased compiler and a few forked libraries. 

To build Meio locally, you can add a temporary opam-repository and use the custom-events compiler:

```
$ opam repo add custom-events https://github.com/TheLortex/custom-events-opam-repository.git
$ opam switch create --no-install custom-events --packages ocaml-variants.5.0.0+custom-events --repositories=custom-events,default
$ opam install --deps-only .
$ dune build
```

You can install meio with `dune install` and then run one of the example programs from Meio.

```
$ meio _build/default/example/burn.exe
```

[Eio]: https://github.com/ocaml-multicore/eio
