open! Base
open Cmdliner
open Cmdliner.Term.Syntax

let run filename code ~param_n =
  let code =
    match param_n with
    | None -> code
    | Some n ->
      (* Slightly hacky way of injecting parameters from the cmdline. *)
      let pattern = "let param_n := 1 in" in
      assert (String.is_substring code ~substring:pattern);
      String.substr_replace_first
        code
        ~pattern
        ~with_:(Printf.sprintf "let param_n := %d in" n)
  in
  let parsed_expr = Language.Parser.parse ~filename code in
  try
    let _value, history =
      Interpreter.Debugger.eval ~break_at_start:false ~source:code parsed_expr
    in
    history
  with
  | Interpreter.Eval.TypeError (msg, value) ->
    Stdio.printf
      "TypeError: Expected value of type `%s` but got `%s`.\n"
      msg
      (Interpreter.Value.to_string value);
    raise Stdlib.Exit
  | Interpreter.Eval.UnhandledEffect (eff, value) ->
    Stdio.printf
      "Unhandled effect while evaluating program: `%s %s`\n"
      (Language.Var.to_string eff)
      (Interpreter.Value.to_string value);
    raise Stdlib.Exit
;;

let prog =
  let doc =
    "$(docv) is a file containing the program. Use $(b,-) for $(b,stdin)."
  in
  let+ file = Arg.(value & pos 0 filepath "-" & info [] ~doc ~docv:"FILE") in
  let filename, code =
    match file with
    | "-" ->
      Stdio.print_string "code> ";
      Out_channel.flush_all ();
      "stdio", Stdio.In_channel.input_line_exn Stdio.stdin
    | file -> file, Stdio.In_channel.read_all file
  in
  filename, String.strip code
;;

let param_n =
  let doc = "$(docv) a parameter for the program passed as n." in
  Arg.(value & pos 1 (some int) None & info [] ~doc)
;;

let main_cmd =
  let doc = "Evaluate a program under a debugger." in
  Cmd.make (Cmd.info "debug" ~doc)
  @@
  let+ filename, code = prog
  and+ param_n = param_n in
  let history = run filename code ~param_n in
  let module Obj = Stdlib.Obj in
  let history_memory = Obj.reachable_words (Obj.repr history) in
  (* Assuming 64-bit words *)
  Stdio.printf "Memory usage for `history`: %d B\n" (history_memory * 8);
  Stdio.printf "Number of states: %d\n" (List.length history)
;;

let () =
  if not !Sys.interactive
  then begin
    Out_channel.set_buffered Stdio.stdout false;
    Stdlib.exit (Cmd.eval main_cmd)
  end
;;
