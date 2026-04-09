# +8 for rounding

for n in {0..5}
do
    echo "Iteration $n:"

    echo "Program A"
    opam exec -- dune exec --no-print-directory ./stats.exe -- ../examples/list.mll "$n"
    echo

    echo "Program B"
    arg2=$(((5538 * n + 2946 + 8) / 16))
    opam exec -- dune exec --no-print-directory ./stats.exe -- ../examples/counter.mll "$arg2"
    echo

    echo "Program C"
    arg3=$(((5538 * n + 18 + 8) / 16))
    opam exec -- dune exec --no-print-directory ./stats.exe -- ../examples/counter_and_list.mll "$arg3"
    echo
done

