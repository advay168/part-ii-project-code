open! Base

let%expect_test "Test parser" =
  Parser.parse ~filename:"test" "1 + 2"
  |> Parser.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    " 
 (MkAdd (MkInt 1 <test:{1:1..1:2}>) (MkInt 2 <test:{1:5..1:6}>)
  <test:{1:1..1:6}>)
 "]
;;
