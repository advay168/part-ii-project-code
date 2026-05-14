# Cambridge Computer Science Tripos Part II Project

This project contains the source code for the Part II Project titled `A better
debugger for rich languages`. It contains an interpreter for the Effektra
language alongside a debugger integrated with it.

## Features
- Implements Effektra which is a functional language with algebraic effects.
- Interpreter based on the CEK abstract machine.
- Debugger supports the following features:
    - Set breakpoint at location
    - Set breakpoint on function call or performing an effect
    - Step the CEK machine
    - Display the internal state of the CEK machine
    - Step-over subexpression computation
    - Time-travel backwards to rewind execution

## Project layout
The project has the following top-level structure:
- `bin/`: Entrypoint for the Effektra interpreter and debugger.
- `language/`: Interpreter frontend that deals with Effektra source code.
- `interpreter/`: Contains code for evaluating Effektra ASTs with the CEK machine.
- `tests/`: Test suite validating the correctness of the interpreter.
- `examples/`: Effektra source files demonstrating various language features or algebraic effects.

## Requirements
- `opam` (Package manager)

## Build/Run instructions
```bash
$ opam install . --deps-only # install libraries

$ make build

$ make examples
# Runs examples

$ make run
> {Enter code here}

$ make debug
> {Enter code here to run it under the debugger}
```

## Development instructions

### Formatting 
Requires `ocamlformat` (Formatter)  
`$ make format`

### Run tests and promote corrections
`$ make test; make promote`

### Miscellaneous
- Create a new OCaml module: `$ dune init lib <name> <dir> --libs base --ppxs ppx_jane <...> <--inline-test> `
- Counting lines of code: `rm -rf _build/ && cloc .`
