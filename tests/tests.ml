open! Base
include Interpreter

let test_string str =
  Language.Ast.show_locs := false;
  let parsed = Language.Parser.parse ~filename:"test" str in
  parsed
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_string;
  Stdio.print_string " --> ";
  Eval.eval ~debug:false parsed |> Value.string_of_t |> Stdio.print_endline
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
  test_string "if true then 1 else 2 end";
  [%expect "(MkIf (MkBool true) (MkInt 1) (MkInt 2)) --> 1"];
  test_string "if false then 1 else 2 end";
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
   | Eval.TypeError _ -> ());
  [%expect {| (MkBinOp (MkNot (MkInt 1)) IEql (MkInt 1)) --> |}]
;;

let%expect_test "Test tuples" =
  test_string "()";
  [%expect "(MkUnit) --> ()"];
  test_string "(1, 3)";
  [%expect "(MkBinOp (MkInt 1) EMkTuple (MkInt 3)) --> (1, 3)"];
  test_string "(1 + 1, 3)";
  [%expect
    "(MkBinOp (MkBinOp (MkInt 1) IAdd (MkInt 1)) EMkTuple (MkInt 3)) --> (2, 3)"];
  test_string "fst@(1, 3)";
  [%expect "(MkApply (MkVar fst) (MkBinOp (MkInt 1) EMkTuple (MkInt 3))) --> 1"];
  test_string "snd@(1, 3)";
  [%expect "(MkApply (MkVar snd) (MkBinOp (MkInt 1) EMkTuple (MkInt 3))) --> 3"]
;;

let%expect_test "Let binds" =
  test_string "let x := 123 in x + 1 end";
  [%expect "(MkLet x (MkInt 123) (MkBinOp (MkVar x) IAdd (MkInt 1))) --> 124"];
  test_string "let x := 123 in let y := 456 in x end end";
  [%expect "(MkLet x (MkInt 123) (MkLet y (MkInt 456) (MkVar x))) --> 123"];
  test_string "let x := 123 in let y := 456 in y end end";
  [%expect "(MkLet x (MkInt 123) (MkLet y (MkInt 456) (MkVar y))) --> 456"];
  test_string "let x := 123 in let x := 456 in x end end";
  [%expect "(MkLet x (MkInt 123) (MkLet x (MkInt 456) (MkVar x))) --> 456"];
  (try test_string "x" with
   | Eval.UnboundVarError _ -> ());
  [%expect {| (MkVar x) --> |}];
  (try test_string "let x := x in x end" with
   | Eval.UnboundVarError _ -> ());
  [%expect {| (MkLet x (MkVar x) (MkVar x)) --> |}]
;;

let%expect_test "Functions" =
  test_string "let f := fun x -> x + 1 end in f end";
  [%expect
    {| (MkLet f (MkFun x (MkBinOp (MkVar x) IAdd (MkInt 1))) (MkVar f)) --> <fun> |}];
  test_string "let f := fun x -> x + 1 end in f@123 end";
  [%expect
    {|
    (MkLet f (MkFun x (MkBinOp (MkVar x) IAdd (MkInt 1)))
     (MkApply (MkVar f) (MkInt 123))) --> 124
    |}];
  test_string "let f := fun f -> f end in f end";
  [%expect {| (MkLet f (MkFun f (MkVar f)) (MkVar f)) --> <fun> |}];
  test_string
    {|
      let fact := fun x ->
        if x = 0 then 1 else x * fact@(x+-1) end
      end in
      fact@5
      end
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
      let addn := fun x -> fun y -> x+y end end in
      let add5 := addn@5 in
      let add6 := addn@6 in
      add5@10 + add6@20
      end
      end
      end
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
  test_string "handle 456 with _, arg, k -> 123 end";
  [%expect
    {| (MkHandle (MkInt 456) ((eff _) (arg arg) (kont k) (body (MkInt 123)))) --> 456 |}];
  test_string "handle perform (Exn 456) with Exn, arg, k -> 123 end";
  [%expect
    {|
    (MkHandle (MkPerform Exn (MkInt 456))
     ((eff Exn) (arg arg) (kont k) (body (MkInt 123)))) --> 123
    |}];
  test_string "handle perform (Exn 456) with Exn, arg, k -> arg end";
  [%expect
    {|
    (MkHandle (MkPerform Exn (MkInt 456))
     ((eff Exn) (arg arg) (kont k) (body (MkVar arg)))) --> 456
    |}];
  test_string "handle 1 + perform (Exn 456) with Exn, arg, k -> arg end";
  [%expect
    {|
    (MkHandle (MkBinOp (MkInt 1) IAdd (MkPerform Exn (MkInt 456)))
     ((eff Exn) (arg arg) (kont k) (body (MkVar arg)))) --> 456
    |}];
  (try test_string "perform (Exn 123)" with
   | Eval.UnhandledEffect _ -> ());
  [%expect {| (MkPerform Exn (MkInt 123)) --> |}];
  (try
     test_string
       "handle 1 + perform (Exn 456) with Exn, arg, k -> perform (Exn 123) end"
   with
   | Eval.UnhandledEffect _ -> ());
  [%expect
    {|
    (MkHandle (MkBinOp (MkInt 1) IAdd (MkPerform Exn (MkInt 456)))
     ((eff Exn) (arg arg) (kont k) (body (MkPerform Exn (MkInt 123))))) -->
    |}]
;;

let%expect_test "Single/Multi-shot Effects" =
  test_string "handle 456 with Eff, arg, k -> k@123 end";
  [%expect
    {|
    (MkHandle (MkInt 456)
     ((eff Eff) (arg arg) (kont k) (body (MkApply (MkVar k) (MkInt 123))))) --> 456
    |}];
  test_string "handle perform (Eff 456) with Eff, arg, k -> k@123 end";
  [%expect
    {|
    (MkHandle (MkPerform Eff (MkInt 456))
     ((eff Eff) (arg arg) (kont k) (body (MkApply (MkVar k) (MkInt 123))))) --> 123
    |}];
  test_string
    "handle perform (Eff 456) with Eff, arg, k -> if arg = 456 then k@123 else \
     perform (Eff 1) end end";
  [%expect
    {|
    (MkHandle (MkPerform Eff (MkInt 456))
     ((eff Eff) (arg arg) (kont k)
      (body
       (MkIf (MkBinOp (MkVar arg) IEql (MkInt 456))
        (MkApply (MkVar k) (MkInt 123)) (MkPerform Eff (MkInt 1)))))) --> 123
    |}];
  test_string
    "handle 2 * perform (Eff 456) with Eff, arg, k -> k@123 + k@789 end";
  [%expect
    {|
    (MkHandle (MkBinOp (MkInt 2) IMul (MkPerform Eff (MkInt 456)))
     ((eff Eff) (arg arg) (kont k)
      (body
       (MkBinOp (MkApply (MkVar k) (MkInt 123)) IAdd
        (MkApply (MkVar k) (MkInt 789)))))) --> 1824
    |}];
  test_string
    "handle perform (Eff 456) + perform (Eff 456) with Eff, arg, k -> k@123 end";
  [%expect
    {|
    (MkHandle
     (MkBinOp (MkPerform Eff (MkInt 456)) IAdd (MkPerform Eff (MkInt 456)))
     ((eff Eff) (arg arg) (kont k) (body (MkApply (MkVar k) (MkInt 123))))) --> 246
    |}]
;;

let%expect_test "Named Effects" =
  test_string "handle perform (Eff1 ()) with Eff1, arg, k -> k@123 end";
  [%expect
    {|
    (MkHandle (MkPerform Eff1 (MkUnit))
     ((eff Eff1) (arg arg) (kont k) (body (MkApply (MkVar k) (MkInt 123))))) --> 123
    |}];
  test_string
    "handle handle perform (Eff2 ()) with Eff1, arg1, k1 -> k1@123 end with \
     Eff2, arg2, k2 -> k2@456 end";
  [%expect
    {|
    (MkHandle
     (MkHandle (MkPerform Eff2 (MkUnit))
      ((eff Eff1) (arg arg1) (kont k1) (body (MkApply (MkVar k1) (MkInt 123)))))
     ((eff Eff2) (arg arg2) (kont k2) (body (MkApply (MkVar k2) (MkInt 456))))) --> 456
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
