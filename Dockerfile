FROM ocaml/opam:alpine-ocaml-5.0@sha256:3ddad7dfb7321d086f7be0fa3807630a8eeb098a5ff7ddfb3d28fc3c853559bf
RUN sudo apk add --update cmake libffi-dev linux-headers zlib-dev
WORKDIR /home/opam/src/ocaml
RUN git clone https://github.com/ocaml/ocaml . && git checkout 61f10168da60e94a5f9c2d1ce4cc4e4d512d0007
RUN opam switch create --empty custom-events && opam pin add ocaml-variants.5.1.0+custom-events . -y
WORKDIR /home/opam/src/meio
COPY --chown=opam meio.opam /home/opam/src/meio/
RUN opam pin . -yn && opam install . --deps-only --with-test
COPY --chown=opam . /home/opam/src/meio/
RUN opam exec -- dune build