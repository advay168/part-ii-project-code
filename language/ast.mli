(* Controls whether the source location is present when converting to sexp. *)
val show_anns : bool ref
val without_showing_anns : (unit -> 'a) -> 'a

type span = Lexing.position * Lexing.position

val linecol_of_span : span -> (int * int) * (int * int)

type 't annotated =
  { span : span
  ; x : 't
  ; mutable breakpoint : bool
  }
[@@deriving sexp_of]

val split_source_by_expr: string -> 'a annotated -> string * string * string

val make : Lexing.position * Lexing.position -> 'a -> 'a annotated

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
  | MkPerform of Var.t * expr
  | MkHandle of expr * handler list

and handler =
  { eff : Var.t
  ; arg : Var.t
  ; kont : Var.t
  ; body : expr
  }
[@@deriving sexp_of]

val mark_breakpoint : int * int -> expr -> bool
