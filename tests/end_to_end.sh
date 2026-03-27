set -e

run-example() {
  opam exec -- dune exec --no-print-directory --no-build bin/main.exe -- eval "$1"
}

check-example-with-expected() {
  diff -w <(run-example "$1") <(echo 'Evaluated:' "$2")
}

# Build it once to save time
opam exec -- dune build --no-print-directory bin/main.exe

check-example-with-expected examples/calc.mll      '46'
check-example-with-expected examples/fact.mll      '120'
check-example-with-expected examples/sum_eff.mll   '10'
check-example-with-expected examples/amb_eff.mll   '(3, 5)'
check-example-with-expected examples/state_eff.mll '246'

echo "All end to end tests passed"
