%{
  open Ast
%}

%token <int> INT
%token PLUS
%token MULT

%token <bool> BOOL
%token EQUALS
%token BAND BOR BNOT
%token IF THEN ELSE

%token LPAREN RPAREN

%token <string> IDENT
%token LET DEF_EQUALS IN

%token FUN ARROW APPLY

%token PERFORM
%token HANDLE WITH

%token END

%token EOF


%left BOR
%left BAND
%left EQUALS
%nonassoc BNOT

%left PLUS
%left MULT

%left APPLY

%start <Ast.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr; EOF { Some e }

expr:  
  | int = INT                                                    { { loc = $loc; e = MkInt     (int)                } }
  | e1 = expr; PLUS; e2 = expr                                   { { loc = $loc; e = MkBinOp   (e1, IAdd, e2)       } }
  | e1 = expr; MULT; e2 = expr                                   { { loc = $loc; e = MkBinOp   (e1, IMul, e2)       } }
  | bool = BOOL                                                  { { loc = $loc; e = MkBool    (bool)               } }
  | e1 = expr; BOR; e2 = expr                                    { { loc = $loc; e = MkBinOp   (e1, BOr, e2)        } }
  | e1 = expr; BAND; e2 = expr                                   { { loc = $loc; e = MkBinOp   (e1, BAnd, e2)       } }
  | BNOT; e = expr                                               { { loc = $loc; e = MkNot     (e)                  } }
  | e1 = expr; EQUALS; e2 = expr                                 { { loc = $loc; e = MkBinOp   (e1, IEql, e2)       } }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; END         { { loc = $loc; e = MkIf      (e1, e2, e3)         } }
  | name = IDENT                                                 { { loc = $loc; e = MkVar     (name)               } }
  | LET; name = IDENT; DEF_EQUALS; e1 = expr; IN; e2 = expr; END { { loc = $loc; e = MkLet     (name, e1, e2)       } }
  | FUN; name = IDENT; ARROW; e = expr; END                      { { loc = $loc; e = MkFun     (name, e)            } }
  | e1 = expr; APPLY; e2 = expr                                  { { loc = $loc; e = MkApply   (e1, e2)             } }
  | PERFORM; LPAREN; e = expr; RPAREN                            { { loc = $loc; e = MkPerform (e)                  } }
  | HANDLE; e1 = expr; WITH;
    name = IDENT; kont = IDENT; ARROW; e2 = expr; END            { { loc = $loc; e = MkHandle  (e1, name, kont, e2) } }
  | LPAREN; e = expr; RPAREN                                     { { loc = $loc; e = e.e                            } }
