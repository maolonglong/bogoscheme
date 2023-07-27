.DEFAULT_GOAL := build

all: fmt test examples

fmt:
	@dune fmt
.PHONY: fmt

build:
	@dune build
.PHONY: build

watch:
	dune build --watch --terminal-persistence=clear-on-rebuild
.PHONY: watch

test:
	@dune test
.PHONY: test

examples:
	dune exec ./main.exe -- ./examples/factorial.bs
	dune exec ./main.exe -- ./examples/fibonacci.bs
	dune exec ./main.exe -- ./examples/test_factorial.bs
	dune exec ./main.exe -- ./examples/test_intdef.bs
	dune exec ./main.exe -- ./examples/test_lambda.bs
	dune exec ./main.exe -- ./examples/test_multi.bs
	dune exec ./main.exe -- ./examples/test_primitives.bs
	dune exec ./main.exe -- ./examples/test_scoping.bs
	dune exec ./main.exe -- ./examples/test_my_lib.bs
.PHONY: examples

clean:
	@dune clean
.PHONY: clean

embed:
	ocaml-embed-file -output embed ./primitives.bs
	dune fmt >/dev/null 2>&1 || true
.PHONY: embed
