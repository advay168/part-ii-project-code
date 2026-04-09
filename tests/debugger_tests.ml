open! Base

let%expect_test "Test location tracking" =
  Language.Ast.show_anns := true;
  Language.Parser.parse
    ~filename:"test"
    "handle 1 + 12\n + 3 with\n| Exn, arg, k -> 123 end"
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  [%expect
    {|
    (MkHandle
     (MkBinOp
      (MkBinOp (MkInt 1 <1:8..1:9>) IAdd (MkInt 12 <1:12..1:14>) <1:8..1:14>)
      IAdd (MkInt 3 <2:4..2:5>) <1:8..2:5>)
     (((eff 'Exn') (arg 'arg') (kont 'k') (body (MkInt 123 <3:18..3:21>))))
     <1:1..3:25>)
    |}]
;;

(* Breakpoint shown by # on location. *)

let%expect_test "Test setting breakpoint by location" =
  Language.Ast.show_anns := true;
  let expr =
    Language.Parser.parse ~filename:"test" "let foo := 123\nin foo end"
  in
  let pos = 1, 1 in
  Stdio.printf "Set: %b\n" (Language.Ast.mark_breakpoint_loc ~set:true pos expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: true
    (MkLet 'foo' (MkInt 123 <1:12..1:15>) (MkVar 'foo' <2:4..2:7>) <#1:1..2:11>)
    |}];
  let pos = 1, 12 in
  Stdio.printf "Set: %b\n" (Language.Ast.mark_breakpoint_loc ~set:true pos expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: true
    (MkLet 'foo' (MkInt 123 <#1:12..1:15>) (MkVar 'foo' <2:4..2:7>) <#1:1..2:11>)
    |}];
  let pos = 2, 4 in
  Stdio.printf "Set: %b\n" (Language.Ast.mark_breakpoint_loc ~set:true pos expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: true
    (MkLet 'foo' (MkInt 123 <#1:12..1:15>) (MkVar 'foo' <#2:4..2:7>)
     <#1:1..2:11>)
    |}];
  let pos = 2, 14 in
  Stdio.printf "Set: %b\n" (Language.Ast.mark_breakpoint_loc ~set:true pos expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: false
    (MkLet 'foo' (MkInt 123 <#1:12..1:15>) (MkVar 'foo' <#2:4..2:7>)
     <#1:1..2:11>)
    |}];
  let pos = 1, 12 in
  Stdio.printf
    "Unset: %b\n"
    (Language.Ast.mark_breakpoint_loc ~set:false pos expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Unset: true
    (MkLet 'foo' (MkInt 123 <1:12..1:15>) (MkVar 'foo' <#2:4..2:7>) <#1:1..2:11>)
    |}]
;;

let%expect_test "Test setting breakpoint by effect name" =
  Language.Ast.show_anns := true;
  let expr =
    Language.Parser.parse
      ~filename:"test"
      "perform (Eff 123) + perform (Eff 123)"
  in
  Stdio.printf "Set: %d\n" (Language.Ast.mark_perform ~set:true "Eff" expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: 2
    (MkBinOp (MkPerform 'Eff' (MkInt 123 <1:14..1:17>) <#1:1..1:18>) IAdd
     (MkPerform 'Eff' (MkInt 123 <1:34..1:37>) <#1:21..1:38>) <1:1..1:38>)
    |}];
  Stdio.printf "Unset: %d\n" (Language.Ast.mark_perform ~set:false "Eff" expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Unset: 2
    (MkBinOp (MkPerform 'Eff' (MkInt 123 <1:14..1:17>) <1:1..1:18>) IAdd
     (MkPerform 'Eff' (MkInt 123 <1:34..1:37>) <1:21..1:38>) <1:1..1:38>)
    |}]
;;

let%expect_test "Test setting breakpoint by function name" =
  Language.Ast.show_anns := true;
  let expr =
    Language.Parser.parse
      ~filename:"test"
      "let func := fun arg -> arg end in func 123 end"
  in
  Stdio.printf "Set: %d\n" (Language.Ast.mark_fun_app ~set:true "func" expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Set: 1
    (MkLet 'func' (MkFun 'arg' (MkVar 'arg' <1:24..1:27>) <1:13..1:31>)
     (MkApply (MkVar 'func' <1:35..1:39>) (MkInt 123 <1:40..1:43>) <#1:35..1:43>)
     <1:1..1:47>)
    |}];
  Stdio.printf "Unset: %d\n" (Language.Ast.mark_fun_app ~set:false "func" expr);
  expr |> Language.Ast.sexp_of_expr |> Sexp.to_string_hum |> Stdio.print_endline;
  [%expect
    {|
    Unset: 1
    (MkLet 'func' (MkFun 'arg' (MkVar 'arg' <1:24..1:27>) <1:13..1:31>)
     (MkApply (MkVar 'func' <1:35..1:39>) (MkInt 123 <1:40..1:43>) <1:35..1:43>)
     <1:1..1:47>)
    |}]
;;
