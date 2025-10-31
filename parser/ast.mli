type 't annotated = { loc : Lexing.position * Lexing.position; e : 't }

type expr = expr' annotated

and expr' = MkInt of int | MkAdd of expr * expr | MkMult of expr * expr
[@@deriving sexp_of]
