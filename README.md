# Cambridge Computer Science Tripos Part II Project

## Requirements
- `opam` (Package manager)

## Build/Run instructions
```bash
$ opam install . --deps-only # install libraries

$ make build

$ make examples # simplified output:
calc.mll:
((((1) + ((12) * (3))) + (4)) + (if (true) then (5) else (6) endif))
Evaluated: 46

fact.mll:
(let fact := (fun n -> (if ((n) = (0)) then (1) else ((n) * ((fact) @ ((n) + (-1)))) endif) endfun) in ((fact) @ (5)) endlet)
Evaluated: 120

$ make run
> {Enter code here}
```

## Formatting instructions
Requires `ocamlformat` (Formatter)  
`$ opam exec -- dune fmt`

## Run tests and auto promote corrections
`$ opam exec -- dune runtest --auto-promote`

## Miscellaneous
`$ dune init lib <name> <dir> --libs base --ppxs ppx_jane <...> <--inline-test> `
