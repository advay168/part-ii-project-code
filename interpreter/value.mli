type t =
  | VInt of int
  | VBool of bool
  | VFun of Language.Var.t * Language.Ast.expr * t Store.t lazy_t

val string_of_t : t -> string
