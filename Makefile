
.PHONY: all array server

all:
	dune build

array: all
	find . -name "*.events" | xargs rm
	OCAML_RUNTIME_EVENTS_START=1 _build/default/test/arr.exe 257 1000000000
	
server: all
	find . -name "*.events" -maxdepth 1 | xargs basename | xargs dune exec -- ./src/server/main.exe