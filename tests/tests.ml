open! Base

let test_string str =
  Language.Ast.show_locs := false;
  let parsed = Language.Parser.parse ~filename:"test" str in
  parsed
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_string;
  Stdio.print_string " --> ";
  Interpreter.Eval.cek_eval parsed
  |> Interpreter.Value.string_of_t
  |> Stdio.print_endline
;;

let%expect_test "Test simple arithmetic" =
  test_string "1 + 2";
  [%expect "(MkBinOp (MkInt 1) IAdd (MkInt 2)) --> 3"]
;;

let%expect_test "Test arithmetic precedence" =
  test_string "1 + 2 * 3 + 4";
  [%expect
    " \n\
    \ (MkBinOp (MkBinOp (MkInt 1) IAdd (MkBinOp (MkInt 2) IMul (MkInt 3))) IAdd\n\
    \  (MkInt 4)) --> 11\n\
    \ "]
;;

let%expect_test "Test bools" =
  test_string "true";
  [%expect "(MkBool true) --> true"];
  test_string "true && true";
  [%expect "(MkBinOp (MkBool true) BAnd (MkBool true)) --> true"];
  test_string "true || false";
  [%expect "(MkBinOp (MkBool true) BOr (MkBool false)) --> true"];
  test_string "~true";
  [%expect "(MkNot (MkBool true)) --> false"];
  test_string "if true then 1 else 2 endif";
  [%expect "(MkIf (MkBool true) (MkInt 1) (MkInt 2)) --> 1"];
  test_string "if false then 1 else 2 endif";
  [%expect "(MkIf (MkBool false) (MkInt 1) (MkInt 2)) --> 2"]
;;

let%expect_test "Bool operation precedence" =
  test_string "true && true || true";
  [%expect
    "(MkBinOp (MkBinOp (MkBool true) BAnd (MkBool true)) BOr (MkBool true)) \
     --> true"];
  test_string "true || true && true";
  [%expect
    "(MkBinOp (MkBool true) BOr (MkBinOp (MkBool true) BAnd (MkBool true))) \
     --> true"];
  test_string "true || true && true";
  [%expect
    "(MkBinOp (MkBool true) BOr (MkBinOp (MkBool true) BAnd (MkBool true))) \
     --> true"];
  test_string "~true && true";
  [%expect "(MkBinOp (MkNot (MkBool true)) BAnd (MkBool true)) --> false"];
  test_string "1 = 1 && true";
  [%expect
    {| (MkBinOp (MkBinOp (MkInt 1) IEql (MkInt 1)) BAnd (MkBool true)) --> true |}];
  (try test_string "~1 = 1" with
   | Interpreter.Eval.TypeError _ -> ());
  [%expect {| (MkBinOp (MkNot (MkInt 1)) IEql (MkInt 1)) --> |}]
;;

let%expect_test "Let binds" =
  test_string "let x := 123 in x + 1 endlet";
  [%expect "(MkLet x (MkInt 123) (MkBinOp (MkVar x) IAdd (MkInt 1))) --> 124"];
  test_string "let x := 123 in let y := 456 in x endlet endlet";
  [%expect "(MkLet x (MkInt 123) (MkLet y (MkInt 456) (MkVar x))) --> 123"];
  test_string "let x := 123 in let y := 456 in y endlet endlet";
  [%expect "(MkLet x (MkInt 123) (MkLet y (MkInt 456) (MkVar y))) --> 456"];
  test_string "let x := 123 in let x := 456 in x endlet endlet";
  [%expect "(MkLet x (MkInt 123) (MkLet x (MkInt 456) (MkVar x))) --> 456"];
  (try test_string "x" with
   | Interpreter.Eval.UnboundVarError _ -> ());
  [%expect {| (MkVar x) --> |}];
  (try test_string "let x := x in x endlet" with
   | Interpreter.Eval.UnboundVarError _ -> ());
  [%expect {| (MkLet x (MkVar x) (MkVar x)) --> |}]
;;

let%expect_test "Functions" =
  test_string "let f := fun x -> x + 1 endfun in f endlet";
  [%expect
    {| (MkLet f (MkFun x (MkBinOp (MkVar x) IAdd (MkInt 1))) (MkVar f)) --> <fun> |}];
  test_string "let f := fun x -> x + 1 endfun in f@123 endlet";
  [%expect
    {|
    (MkLet f (MkFun x (MkBinOp (MkVar x) IAdd (MkInt 1)))
     (MkApply (MkVar f) (MkInt 123))) --> 124
    |}];
  test_string "let f := fun f -> f endfun in f endlet";
  [%expect {| (MkLet f (MkFun f (MkVar f)) (MkVar f)) --> <fun> |}];
  test_string
    {|
      let fact := fun x ->
        if x = 0 then 1 else x * fact@(x+-1) endif
      endfun in
      fact@5
      endlet
    |};
  [%expect
    {|
    (MkLet fact
     (MkFun x
      (MkIf (MkBinOp (MkVar x) IEql (MkInt 0)) (MkInt 1)
       (MkBinOp (MkVar x) IMul
        (MkApply (MkVar fact) (MkBinOp (MkVar x) IAdd (MkInt -1))))))
     (MkApply (MkVar fact) (MkInt 5))) --> 120
    |}];
  test_string
    {|
      let addn := fun x -> fun y -> x+y endfun endfun in
      let add5 := addn@5 in
      let add6 := addn@6 in
      add5@10 + add6@20
      endlet
      endlet
      endlet
    |};
  [%expect
    {|
    (MkLet addn (MkFun x (MkFun y (MkBinOp (MkVar x) IAdd (MkVar y))))
     (MkLet add5 (MkApply (MkVar addn) (MkInt 5))
      (MkLet add6 (MkApply (MkVar addn) (MkInt 6))
       (MkBinOp (MkApply (MkVar add5) (MkInt 10)) IAdd
        (MkApply (MkVar add6) (MkInt 20)))))) --> 41
    |}]
;;

let%expect_test "Exceptions" =
  test_string "try 456 with exn -> 123 endtry";
  [%expect "(MkTry (MkInt 456) exn (MkInt 123)) --> 456"];
  test_string "try raise (456) with exn -> 123 endtry";
  [%expect "(MkTry (MkRaise (MkInt 456)) exn (MkInt 123)) --> 123"];
  test_string "try raise (456) with exn -> exn endtry";
  [%expect "(MkTry (MkRaise (MkInt 456)) exn (MkVar exn)) --> 456"];
  test_string "try 1 + raise (456) with exn -> exn endtry";
  [%expect
    "(MkTry (MkBinOp (MkInt 1) IAdd (MkRaise (MkInt 456))) exn (MkVar exn)) \
     --> 456"];
  (try test_string "raise (123)" with
   | Interpreter.Eval.LangException _ -> ());
  [%expect {| (MkRaise (MkInt 123)) --> |}];
  (try test_string "try 1 + raise (456) with exn -> raise (123) endtry" with
   | Interpreter.Eval.LangException _ -> ());
  [%expect
    {|
    (MkTry (MkBinOp (MkInt 1) IAdd (MkRaise (MkInt 456))) exn
     (MkRaise (MkInt 123))) -->
    |}]
;;

let%expect_test "Test location tracking" =
  Language.Ast.show_locs := true;
  Language.Parser.parse ~filename:"test" "1 + 12\n + 3"
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  [%expect
    " \n\
    \ (MkBinOp\n\
    \  (MkBinOp (MkInt 1 <test:{1:1..1:2}>) IAdd (MkInt 12 <test:{1:5..1:7}>)\n\
    \   <test:{1:1..1:7}>)\n\
    \  IAdd (MkInt 3 <test:{2:4..2:5}>) <test:{1:1..2:5}>)\n\
    \ "]
;;
