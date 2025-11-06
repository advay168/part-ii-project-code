open! Base

exception ParseError of string

let parse ~filename string =
  let lexbuf = Lexing.from_string string in
  Lexing.set_filename lexbuf filename;
  match Parser_generator.prog Lexer.read lexbuf with
  | Some expr -> expr
  | None -> raise (ParseError "Empty string")
  | exception Parser_generator.Error -> raise (ParseError "Parsing Error")
  | exception Lexer.SyntaxError err ->
    raise (ParseError ("Lexing error: " ^ err))
;;

module Ast = Ast
module Pretty_print = Pretty_print
