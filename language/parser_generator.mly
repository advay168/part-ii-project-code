%{
  open Ast
%}

%token <int> INT
%token PLUS
%token MULT

%token <bool> BOOL
%token EQUALS
%token BAND BOR BNOT
%token IF THEN ELSE ENDIF

%token LPAREN RPAREN

%token <string> IDENT
%token LET DEF_EQUALS IN ENDLET

%token EOF


%left BOR
%left BAND
%left EQUALS
%nonassoc BNOT

%left PLUS
%left MULT

%start <Ast.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr; EOF { Some e }

expr:  
  | int = INT { { loc = $loc; e = MkInt int} }
  | e1 = expr; PLUS; e2 = expr {{ loc = $loc; e = MkAdd (e1, e2)}  }
  | e1 = expr; MULT; e2 = expr {{ loc = $loc; e = MkMult (e1, e2)}  }
  | LPAREN; e = expr; RPAREN { e }
  | bool = BOOL { { loc = $loc; e = MkBool bool} }
  | e1 = expr; BOR; e2 = expr {{ loc = $loc; e = MkOr (e1, e2)}  }
  | e1 = expr; BAND; e2 = expr {{ loc = $loc; e = MkAnd (e1, e2)}  }
  | BNOT; e = expr { {loc = $loc; e = MkNot e} }
  | e1 = expr; EQUALS; e2 = expr {{ loc = $loc; e = MkEqual (e1, e2)}  }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; ENDIF { { loc = $loc; e = MkIf (e1, e2, e3)} }
  | name = IDENT { { loc = $loc; e = MkVar name} }
  | LET; name = IDENT; DEF_EQUALS; e1 = expr; IN; e2 = expr; ENDLET { { loc = $loc; e = MkLet (name, e1, e2)} }