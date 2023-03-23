meio
----

*Status: Very, very experimental & WIP*

If you want to take this for a spin, you'll need to install an OCaml compiler that supports the Custom Events PR https://github.com/ocaml/ocaml/pull/11474. Have a look at the Dockerfile for how you might go about doing that. There is also an image available at https://hub.docker.com/repository/docker/patricoferris/meio if you want to just `docker run` and try out some programs quickly!

[![Meio on asciicast](https://asciinema.org/a/542862.svg)](https://asciinema.org/a/542862)

### Building meio

In a new switch:
```
$ opam repo add custom-events https://github.com/TheLortex/custom-events-opam-repository.git
$ opam switch create --no-install custom-events --packages ocaml-variants.5.0.0+custom-events --repositories=custom-events,default
$ opam install --deps-only .
$ dune build
```
