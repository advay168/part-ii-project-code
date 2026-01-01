{
open Lexing
open Parser_generator

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}
let digit = ['0'-'9']


let alpha_underscore = ['a'-'z' 'A'-'Z' '_']

let ident = alpha_underscore (alpha_underscore|digit)*

let int = '-'? digit+

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { PLUS }
  | '*' { MULT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "&&" { BAND }
  | "||" { BOR }
  | "~" { BNOT }
  | '=' { EQUALS }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | ":=" { DEF_EQUALS }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "@" { APPLY }
  | "perform" { PERFORM }
  | "handle" { HANDLE }
  | "with" { WITH }
  | "|" { BAR }
  | "end" { END }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | newline {  next_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
