open! Base
open Import

let input () =
  let argv = Sys.get_argv () in
  String.strip
    (if Array.length argv = 1 then Stdio.In_channel.input_line_exn Stdio.stdin
     else Stdio.In_channel.read_all argv.(1))
;;

let () =
  let code = input () in
  let parsed_expr = Parser.parse code in
  parsed_expr |> Parser.Ast.sexp_of_expr |> Sexp.to_string_hum
  |> Stdio.print_endline;
  parsed_expr |> Parser.Pretty_print.pp |> Stdio.print_endline;
  let evaluated = Eval.eval parsed_expr in
  Stdio.printf "Evaluated: %i" evaluated
;;
