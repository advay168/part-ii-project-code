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

%token LPAREN RPAREN COMMA

%token <string> IDENT
%token LET DEF_EQUALS IN

%token FUN ARROW APPLY

%token PERFORM
%token HANDLE WITH BAR

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
  | int = INT                                                    { make $loc (MkInt     (int)             ) }
  | bool = BOOL                                                  { make $loc (MkBool    (bool)            ) }
  | LPAREN; RPAREN                                               { make $loc (MkUnit                      ) }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN                  { make $loc (MkBinOp   (e1, EMkTuple, e2)) }
  | e1 = expr; PLUS; e2 = expr                                   { make $loc (MkBinOp   (e1, IAdd, e2)    ) }
  | e1 = expr; MULT; e2 = expr                                   { make $loc (MkBinOp   (e1, IMul, e2)    ) }
  | e1 = expr; BOR; e2 = expr                                    { make $loc (MkBinOp   (e1, BOr, e2)     ) }
  | e1 = expr; BAND; e2 = expr                                   { make $loc (MkBinOp   (e1, BAnd, e2)    ) }
  | BNOT; e = expr                                               { make $loc (MkNot     (e)               ) }
  | e1 = expr; EQUALS; e2 = expr                                 { make $loc (MkBinOp   (e1, IEql, e2)    ) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; END         { make $loc (MkIf      (e1, e2, e3)      ) }
  | name = IDENT                                                 { make $loc (MkVar     (name)            ) }
  | LET; name = IDENT; DEF_EQUALS; e1 = expr; IN; e2 = expr; END { make $loc (MkLet     (name, e1, e2)    ) }
  | FUN; name = IDENT; ARROW; e = expr; END                      { make $loc (MkFun     (name, e)         ) }
  | e1 = expr; APPLY; e2 = expr                                  { make $loc (MkApply   (e1, e2)          ) }
  | PERFORM; LPAREN; eff = IDENT; e = expr; RPAREN               { make $loc (MkPerform (eff, e)          ) }
  | HANDLE; e1 = expr; WITH; hs = nonempty_list(handler); END    { make $loc (MkHandle  (e1, hs)          ) }
  | LPAREN; e = expr; RPAREN                                     { make $loc (e.x                         ) }

handler:
  | BAR; eff = IDENT; COMMA; arg = IDENT; COMMA; kont = IDENT; ARROW; body = expr { {eff; arg; kont; body} }
