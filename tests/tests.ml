open! Base

let%expect_test "Test parser" =
  Parser.parse "1 + 2" |> Parser.Ast.sexp_of_expr |> Sexp.to_string_hum
  |> Stdio.print_endline;
  [%expect "(MkAdd (MkInt 1) (MkInt 2))"]
;;
