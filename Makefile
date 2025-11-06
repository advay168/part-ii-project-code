DUNE = opam exec -- dune

.PHONY: all build watch run nrun repl test clean fmt promote precommit

all: build

build:
	$(DUNE) build

watch:
	$(DUNE) build --watch

run:
	$(DUNE) exec --no-print-directory bin/main.exe -- examples/calc.mll

nrun:
	$(DUNE) exec --no-print-directory --no-build bin/main.exe -- examples/calc.mll

repl:
	$(DUNE) exec --no-print-directory --no-build bin/main.exe

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

fmt:
	$(DUNE) fmt

promote:
	$(DUNE) promote

precommit: build test
	$(DUNE) build @fmt
