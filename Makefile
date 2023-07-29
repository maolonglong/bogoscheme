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
	./bs ./examples/factorial.scm
	./bs ./examples/fibonacci.scm
	./bs ./examples/test_factorial.scm
	./bs ./examples/test_intdef.scm
	./bs ./examples/test_lambda.scm
	./bs ./examples/test_multi.scm
	./bs ./examples/test_primitives.scm
	./bs ./examples/test_scoping.scm
	./bs ./examples/test_my_lib.scm
	./bs ./examples/test_cond.scm
.PHONY: examples

clean:
	@dune clean
.PHONY: clean

embed:
	ocaml-embed-file -output embed -output-dir lib ./primitives.scm
	dune fmt >/dev/null 2>&1 || true
.PHONY: embed
