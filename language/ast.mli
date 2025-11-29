(* Controls whether the source location is present when converting to sexp. *)
val show_locs : bool ref
val without_showing_locs : (unit -> 'a) -> 'a

type 't annotated =
  { loc : Lexing.position * Lexing.position
  ; e : 't
  }

type binOp =
  | IAdd
  | IMul
  | IEql
  | BAnd
  | BOr
[@@deriving sexp_of]

type expr = expr' annotated

and expr' =
  | MkInt of int
  | MkBinOp of expr * binOp * expr
  | MkBool of bool
  | MkNot of expr
  | MkIf of expr * expr * expr
  | MkVar of Var.t
  | MkLet of Var.t * expr * expr
  | MkFun of Var.t * expr
  | MkApply of expr * expr
  | MkRaise of expr
  | MkTry of expr * Var.t * expr
[@@deriving sexp_of]
