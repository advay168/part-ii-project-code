open! Base
open Import
open Cmdliner
open Cmdliner.Term.Syntax

let eval ~debug filename code =
  let parsed_expr = Language.Parser.parse ~filename code in
  parsed_expr
  |> Language.Ast.sexp_of_expr
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  parsed_expr |> Language.Pretty_print.pp |> Stdio.print_endline;
  try
    let evaluated = Eval.cek_eval ~debug parsed_expr in
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

let prog =
  let doc =
    "$(docv) is a file containing the program. Use $(b,-) for $(b,stdin)."
  in
  let+ file = Arg.(value & pos 0 filepath "-" & info [] ~doc ~docv:"FILE") in
  let filename, code =
    match file with
    | "-" ->
      Stdio.print_string "> ";
      Out_channel.flush_all ();
      "stdio", Stdio.In_channel.input_line_exn Stdio.stdin
    | file -> file, Stdio.In_channel.read_all file
  in
  filename, String.strip code
;;

let eval_cmd =
  let doc = "Evaluate a program." in
  Cmd.make (Cmd.info "eval" ~doc)
  @@
  let+ filename, code = prog in
  eval ~debug:false filename code
;;

let debug_cmd =
  let doc = "Evaluate a program under a debugger." in
  Cmd.make (Cmd.info "debug" ~doc)
  @@
  let+ filename, code = prog in
  eval ~debug:true filename code
;;

let main_cmd =
  let default =
    (* show help *)
    Term.(ret (const (`Help (`Auto, None))))
  in
  Cmd.group (Cmd.info "main.exe") ~default [ eval_cmd; debug_cmd ]
;;

let () = if not !Sys.interactive then Stdlib.exit (Cmd.eval main_cmd)
