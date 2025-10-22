%{
  open Ast
%}

%token <int> INT
%token PLUS
%token MULT
%token EOF
%token LPAREN
%token RPAREN

%left PLUS
%left MULT

%start <Ast.expr option> start
%%

start:
  | EOF { None }
  | e = expr; EOF { Some e }

expr:  
  | int = INT { MkInt int }
  | e1 = expr; PLUS; e2 = expr { MkAdd (e1, e2) }
  | e1 = expr; MULT; e2 = expr { MkMult (e1, e2) }
  | LPAREN; e = expr; RPAREN { e }

(* expr:  *)
(*   | t = term { t } *)
(*   | t1 = expr; PLUS; t2 = term {MkAdd (t1, t2)} ; *)
(**)
(* term: *)
(*   | f = factor { f } *)
(*   | f1 = term; MULT; f2 = factor {MkMult (f1, f2)} ; *)
(**)
(* factor: *)
(*   | i = INT {MkInt i} ; *)
(**)
