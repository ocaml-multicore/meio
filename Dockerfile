FROM ocaml/opam:alpine-ocaml-5.0@sha256:7c6d01a1255243c96ab93cc3f41bb68bcacac0393147d7fe54576ff5c43ad459
RUN sudo apk add --update cmake libffi-dev linux-headers zlib-dev
WORKDIR /home/opam/src/ocaml
RUN git clone https://github.com/TheLortex/ocaml . && git checkout d589bf7266ac1e4f264730b87dd54f3cad6495b2
RUN opam switch create --empty custom-events && opam pin add ocaml-variants.5.1.0+custom-events . -y
WORKDIR /home/opam/src/meio
COPY --chown=opam meio.opam /home/opam/src/meio/
RUN opam pin . -yn && opam install . --deps-only --with-test
COPY --chown=opam . /home/opam/src/meio/
RUN opam exec -- dune build