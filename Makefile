
.PHONY: all array server

all:
	dune build

fib: all
	find . -name "*.events" | xargs rm
	OCAML_RUNTIME_EVENTS_START=1 _build/default/test/fib.exe 3
	
server: all
	find . -name "*.events" -maxdepth 1 | xargs basename | xargs dune exec -- ./src/server/main.exe