DUNE = opam exec -- dune

.PHONY: all build watch run examples nexamples test clean fmt promote precommit

all: build

build:
	$(DUNE) build

watch:
	$(DUNE) build --watch

run:
	$(DUNE) exec --no-print-directory --no-build bin/main.exe

examples:
	$(DUNE) exec --no-print-directory bin/main.exe -- examples/calc.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- examples/fact.mll

nexamples:
	$(DUNE) exec --no-print-directory --no-build bin/main.exe -- examples/calc.mll
	$(DUNE) exec --no-print-directory --no-build bin/main.exe -- examples/fact.mll

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

fmt:
	-$(DUNE) fmt

promote:
	$(DUNE) promote

precommit: build test
	$(DUNE) build @fmt
