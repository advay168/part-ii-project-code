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

let int = '-'? digit+
let alpha = ['a'-'z' 'A'-'Z']

let ident = alpha (alpha|digit)*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { PLUS }
  | '*' { MULT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "&&" { BAND }
  | "||" { BOR }
  | "~" { BNOT }
  | '=' { EQUALS }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "endif" { ENDIF }
  | "let" { LET }
  | ":=" { DEF_EQUALS }
  | "in" { IN }
  | "endlet" { ENDLET }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | newline {  next_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
