set -e

example() {
  opam exec -- dune exec --no-print-directory --no-build bin/main.exe -- eval "examples/$1"
}

check-against-expected() {
  diff -w "$1" <(echo 'Evaluated:' "$2")
}

# Build it once to save time
opam exec -- dune build --no-print-directory bin/main.exe

check-against-expected <(example amb_eff.mll)   '(3, 5)'
check-against-expected <(example calc.mll)      '46'
check-against-expected <(example fact.mll)      '120'
check-against-expected <(example state_eff.mll) '246'
check-against-expected <(example sum_eff.mll)   '10'

check-against-expected <(example list.mll | head -c 47 && echo) \
  '(true, (101, (true, (100, (true, (99'

diff -w <(example printing.mll) <(echo -e '123\n456\n789\nEvaluated: ()')

check-against-expected <(example input.mll < <(echo -e "123\\n456\\n")) '579'

diff -w <(example interactive.mll < <(echo -e "123\\n456\\n789\\n")) <(echo -e '579\n1368\nEvaluated: ()')

echo "All end to end tests passed"
