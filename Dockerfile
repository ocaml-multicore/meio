FROM ocaml/opam:alpine-ocaml-5.0@sha256:5cfaa996380122a4f00436040c2549115c08d01eebfd17aaa96b90186e37c344
RUN sudo apk add --update cmake libffi-dev linux-headers zlib-dev
WORKDIR /home/opam/src/ocaml
RUN opam repo add custom-events https://github.com/TheLortex/custom-events-opam-repository.git
RUN opam switch create --no-install custom-events --packages ocaml-variants.5.0.0+custom-events --repositories=custom-events,default
WORKDIR /home/opam/src/meio
COPY --chown=opam meio.opam /home/opam/src/meio/
RUN opam pin . -yn && opam install . --deps-only --with-test
COPY --chown=opam . /home/opam/src/meio/
RUN opam exec -- dune build
