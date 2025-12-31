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
  | EMkTuple
[@@deriving sexp_of]

type expr = expr' annotated

and expr' =
  | MkInt of int
  | MkBool of bool
  | MkUnit
  | MkBinOp of expr * binOp * expr
  | MkNot of expr
  | MkIf of expr * expr * expr
  | MkVar of Var.t
  | MkLet of Var.t * expr * expr
  | MkFun of Var.t * expr
  | MkApply of expr * expr
  | MkPerform of expr
  | MkHandle of expr * Var.t * Var.t * expr
[@@deriving sexp_of]
