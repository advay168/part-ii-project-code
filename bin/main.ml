open! Base
open Import

let input () =
  let argv = Sys.get_argv () in
  let filename, code =
    if Array.length argv = 1
    then (
      Stdio.print_string "> ";
      Out_channel.flush_all ();
      "stdio", Stdio.In_channel.input_line_exn Stdio.stdin)
    else argv.(1), Stdio.In_channel.read_all argv.(1)
  in
  filename, String.strip code
;;

let () =
  let filename, code = input () in
  let parsed_expr = Language.Parser.parse ~filename code in
  parsed_expr
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  parsed_expr |> Language.Pretty_print.pp |> Stdio.print_endline;
  try
    let evaluated = Eval.eval (Store.empty, parsed_expr) in
    Stdio.printf "Evaluated: %s\n" (Value.string_of_t evaluated)
  with
  | Eval.TypeError (msg, value) ->
    Stdio.printf
      "TypeError: Expected value of type `%s` but got `%s`.\n"
      msg
      (Value.string_of_t value)
  | Eval.LangException value ->
    Stdio.printf
      "Unhandled exception while evaluating program: `%s`\n"
      (Value.string_of_t value)
;;
