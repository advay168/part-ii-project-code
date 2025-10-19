# Cambridge Computer Science Tripos Part II Project

## Requirements
- `opam` (Package manager)
- `dune` (Build system)
- `ocamlc` (Compiler)
- `ocamlformat` (Formatter)

## Build/Run instructions
`$ opam install . --deps-only`

`$ opam exec -- dune build`

`$ opam exec -- dune exec --no-print-directory bin/main.exe -- examples/fact.mll`

## Formatting instructions
`$ opam exec -- dune fmt`

## Run tests and auto promote corrections
`$ opam exec -- dune runtest --auto-promote`

## Miscellaneous
`$ dune init lib <name> <dir> --libs base --ppxs ppx_jane <...> <--inline-test> `
