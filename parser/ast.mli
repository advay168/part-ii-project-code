(* Controls whether the source location is present when converting to sexp. *)
val show_locs : bool ref

type 't annotated =
  { loc : Lexing.position * Lexing.position
  ; e : 't
  }

type expr = expr' annotated

and expr' =
  | MkInt of int
  | MkAdd of expr * expr
  | MkMult of expr * expr
  | MkBool of bool
  | MkAnd of expr * expr
  | MkOr of expr * expr
  | MkNot of expr
  | MkEqual of expr * expr
  | MkIf of expr * expr * expr
[@@deriving sexp_of]
