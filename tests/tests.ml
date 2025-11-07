open! Base

let test_string str =
  Language.Ast.show_locs := false;
  let parsed = Language.Parser.parse ~filename:"test" str in
  parsed
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_string;
  Stdio.print_string " --> ";
  Interpreter.Eval.eval (Interpreter.Store.empty, parsed)
  |> Interpreter.Value.string_of_t
  |> Stdio.print_endline
;;

let%expect_test "Test simple arithmetic" =
  test_string "1 + 2";
  [%expect "(MkAdd (MkInt 1) (MkInt 2)) --> 3"]
;;

let%expect_test "Test arithmetic precedence" =
  test_string "1 + 2 * 3 + 4";
  [%expect
    "(MkAdd (MkAdd (MkInt 1) (MkMult (MkInt 2) (MkInt 3))) (MkInt 4)) --> 11"]
;;

let%expect_test "Test bools" =
  test_string "true";
  [%expect "(MkBool true) --> true"];
  test_string "true && true";
  [%expect "(MkAnd (MkBool true) (MkBool true)) --> true"];
  test_string "true || false";
  [%expect "(MkOr (MkBool true) (MkBool false)) --> true"];
  test_string "~true";
  [%expect "(MkNot (MkBool true)) --> false"];
  test_string "if true then 1 else 2 endif";
  [%expect "(MkIf (MkBool true) (MkInt 1) (MkInt 2)) --> 1"];
  test_string "if false then 1 else 2 endif";
  [%expect "(MkIf (MkBool false) (MkInt 1) (MkInt 2)) --> 2"]
;;

let%expect_test "Bool operation precedence" =
  test_string "true && true || true";
  [%expect "(MkOr (MkAnd (MkBool true) (MkBool true)) (MkBool true)) --> true"];
  test_string "true || true && true";
  [%expect "(MkOr (MkBool true) (MkAnd (MkBool true) (MkBool true))) --> true"];
  test_string "true || true && true";
  [%expect "(MkOr (MkBool true) (MkAnd (MkBool true) (MkBool true))) --> true"];
  test_string "~true && true";
  [%expect "(MkAnd (MkNot (MkBool true)) (MkBool true)) --> false"];
  test_string "1 = 1 && true";
  [%expect {| (MkAnd (MkEqual (MkInt 1) (MkInt 1)) (MkBool true)) --> true |}];
  (try test_string "~1 = 1" with
   | Interpreter.Eval.TypeError _ -> ());
  [%expect {| (MkEqual (MkNot (MkInt 1)) (MkInt 1)) --> |}]
;;

let%expect_test "Test location tracking" =
  Language.Ast.show_locs := true;
  Language.Parser.parse ~filename:"test" "1 + 12\n + 3"
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  [%expect
    " \n\
    \ (MkAdd\n\
    \  (MkAdd (MkInt 1 <test:{1:1..1:2}>) (MkInt 12 <test:{1:5..1:7}>)\n\
    \   <test:{1:1..1:7}>)\n\
    \  (MkInt 3 <test:{2:4..2:5}>) <test:{1:1..2:5}>)\n\
    \ "]
;;
