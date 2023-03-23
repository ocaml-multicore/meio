FROM ocaml/opam:alpine-ocaml-5.0@sha256:3ddad7dfb7321d086f7be0fa3807630a8eeb098a5ff7ddfb3d28fc3c853559bf
RUN sudo apk add --update cmake libffi-dev linux-headers zlib-dev
WORKDIR /home/opam/src/ocaml
RUN opam repo add custom-events https://github.com/TheLortex/custom-events-opam-repository.git
RUN opam switch create --no-install custom-events --packages ocaml-variants.5.0.0+custom-events
WORKDIR /home/opam/src/meio
COPY --chown=opam meio.opam /home/opam/src/meio/
RUN opam pin . -yn && opam install . --deps-only --with-test
COPY --chown=opam . /home/opam/src/meio/
RUN opam exec -- dune build
