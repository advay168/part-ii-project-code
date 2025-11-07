type t =
  | VInt of int
  | VBool of bool
  | VFun of Language.Var.t * Language.Ast.expr * t Store.t lazy_t

let string_of_t = function
  | VInt int -> Int.to_string int
  | VBool bool -> Bool.to_string bool
  | VFun _ -> "<fun>"
;;
