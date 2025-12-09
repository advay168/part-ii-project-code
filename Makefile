DUNE = opam exec -- dune

.PHONY: all build watch run examples nexamples test clean fmt promote precommit

all: build

build:
	$(DUNE) build

watch:
	$(DUNE) build --watch

run:
	$(DUNE) exec --no-print-directory bin/main.exe -- eval

debug:
	$(DUNE) exec --no-print-directory bin/main.exe -- debug

examples:
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/calc.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/fact.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/sum_eff.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/amb_eff.mll

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
