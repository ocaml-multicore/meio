eio-console
-----------

*Status: Very experimental & WIP*

With the upcoming release of OCaml 5, OCaml programs will have built-in parallelism and the ability to offer direct-style concurrency using effects like that provided by [Eio][].

Providing tools to introspect and debug these programs is crucial. Along with domains and effects, OCaml 5 is also likely to get a new profiling tool called [Eventring](). This is an efficient, always-on tool for profiling *live* OCaml programs. Currently, it does not support custom events so only lifetime and GC events are supported. 

Eio-console provides an application for monitoring running programs. This works in the browser, communicating information over a websocket. It could be extended to run directly in the terminal using [nottui]().

## Bindings

The `bindings` directory contains partial jsoo bindings to various JavaScript libraries which are themselves vendorered statically. See the directories for the various the licenses for those libraries.

[Eio]: https://github.com/ocaml-multicore/eio