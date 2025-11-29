open! Base

type t =
  | VInt of int
  | VBool of bool
  | VFun of Language.Var.t * Language.Ast.expr * t Env.t lazy_t

let string_of_t = function
  | VInt int -> Int.to_string int
  | VBool bool -> Bool.to_string bool
  | VFun _ -> "<fun>"
;;

let sexp_of_t t = Sexp.Atom (string_of_t t)
