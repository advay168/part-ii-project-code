# Cambridge Computer Science Tripos Part II Project

## Requirements
- `opam` (Package manager)

## Build/Run instructions
```bash
$ opam install . --deps-only # install libraries

$ opam exec -- dune build

$ cat examples/calc.mll
1 + 2 * 3 + 4

$ opam exec -- dune exec --no-print-directory bin/main.exe -- examples/calc.mll
(MkAdd (MkAdd (MkInt 1) (MkMult (MkInt 2) (MkInt 3))) (MkInt 4))
```

## Formatting instructions
Requires `ocamlformat` (Formatter)  
`$ opam exec -- dune fmt`

## Run tests and auto promote corrections
`$ opam exec -- dune runtest --auto-promote`

## Miscellaneous
`$ dune init lib <name> <dir> --libs base --ppxs ppx_jane <...> <--inline-test> `
