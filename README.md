eio-console
-----------

*Status: Very experimental & WIP*

With the upcoming release of OCaml 5, OCaml programs will have built-in parallelism and the ability to offer direct-style concurrency using effects like that provided by [Eio][].

Providing tools to introspect and debug these programs is crucial. Along with domains and effects, OCaml 5 is also likely to get a new profiling tool called [Eventring](https://github.com/ocaml/ocaml/pull/10964). This is an efficient, always-on tool for profiling *live* OCaml programs. Currently, it does not support custom events so only lifetime and GC events are supported.

Eio-console provides an application for monitoring running programs. This works in the browser, communicating information over a websocket. It could be extended to run directly in the terminal using [nottui](https://github.com/let-def/lwd).

## Installation

In the pre-OCaml 5 world, installation is not easy. The best way to get a compatible compiler is to use the excellent opam-plugin [opam-compiler](). At the time of writing you can then create an Eventring-ready switch with:

```
opam compiler create '#10964'
opam switch set-invariant ocaml-variants.5.0.0+trunk
```

After that it is recommended to install the alpha repository:

```
opam repo add alpha git+https://github.com/kit-ty-kate/opam-alpha-repository.git
opam update
```

Finally you can install the dependencies for the project. 

```
opam pin . -yn
opam install . --deps-only --with-test
```

Hopefully that will get all of the necessary dependencies so you can then run `dune build`.

## Usage

To use eio-console right now, it only supports monitoring a single eventring that you must specify. First, you have to run some program with eventring enabled. The executables in the `test` directory can be used for this:

```
OCAML_EVENTRING_START=1 _build/default/test/fib.exe 2 50
```

This will run the program and their should be a `<pid>.eventring` file in the directory that you ran the program from. You can then run

```
dune exec -- src/server/main.exe <pid>.eventring
```

and navigate to http://localhost:8080 to see the dashboard.


## Bindings

The `bindings` directory contains partial jsoo bindings to various JavaScript libraries which are themselves vendorered statically. See the directories for the various the licenses for those libraries.

[Eio]: https://github.com/ocaml-multicore/eio