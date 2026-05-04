SHELL := /bin/bash
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
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/amb_eff.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/calc.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/counter.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/counter_and_list.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/fact.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/input.mll < <(echo -e "123\\n456\\n")
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/interactive.mll < <(echo -e "123\\n456\\n789\\n")
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/list.mll | head -c 95 && echo
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/printing.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/state_eff.mll
	$(DUNE) exec --no-print-directory bin/main.exe -- eval examples/sum_eff.mll

test:
	$(DUNE) runtest
	bash tests/end_to_end.sh

clean:
	$(DUNE) clean

fmt:
	-$(DUNE) fmt

promote:
	$(DUNE) promote

precommit: build test
	$(DUNE) build @fmt
