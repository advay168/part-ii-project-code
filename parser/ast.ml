open Base

type expr = MkInt of int | MkAdd of expr * expr | MkMult of expr * expr
[@@deriving sexp]
