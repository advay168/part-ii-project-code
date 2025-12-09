# Cambridge Computer Science Tripos Part II Project

## Requirements
- `opam` (Package manager)

## Build/Run instructions
```bash
$ opam install . --deps-only # install libraries

$ make build

$ make examples
# Runs `calc.mll`
# Runs `fact.mll`
# Runs `sum_eff.mll`
# Runs `amb_eff.mll`

$ make run
> {Enter code here}

$ make debug
> {Enter code here to run it under the debugger}
```

## Formatting instructions
Requires `ocamlformat` (Formatter)  
`$ opam exec -- dune fmt`

## Run tests and auto promote corrections
`$ opam exec -- dune runtest --auto-promote`

## Miscellaneous
`$ dune init lib <name> <dir> --libs base --ppxs ppx_jane <...> <--inline-test> `
