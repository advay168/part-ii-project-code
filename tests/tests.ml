let%expect_test "Test parser" =
  Parser.parse "1 + 2" |> Stdio.print_endline;
  [%expect "1 + 2"]
;;
