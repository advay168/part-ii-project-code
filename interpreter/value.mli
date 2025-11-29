type t =
  | VInt of int
  | VBool of bool
  | VFun of Language.Var.t * Language.Ast.expr * t Env.t lazy_t
[@@deriving sexp_of]

val string_of_t : t -> string
