open! Base

let as_int = function
  | Value.VInt int -> int
  | _ -> failwith "Type mismatch"
;;

let as_bool = function
  | Value.VBool bool -> bool
  | _ -> failwith "Type mismatch"
;;

let equal = function
  | Value.VInt value1, Value.VInt value2 -> Int.equal value1 value2
  | _ -> failwith "Type mismatch"
;;

let rec eval (expr : Parser.Ast.expr) : Value.t =
  match expr.e with
  | Parser.Ast.MkInt int -> VInt int
  | MkAdd (expr1, expr2) -> VInt (as_int (eval expr1) + as_int (eval expr2))
  | MkMult (expr1, expr2) -> VInt (as_int (eval expr1) * as_int (eval expr2))
  | MkBool bool -> VBool bool
  | MkAnd (expr1, expr2) -> VBool (as_bool (eval expr1) && as_bool (eval expr2))
  | MkOr (expr1, expr2) -> VBool (as_bool (eval expr1) || as_bool (eval expr2))
  | MkNot expr -> VBool (not (as_bool (eval expr)))
  | MkEqual (expr1, expr2) -> VBool (equal (eval expr1, eval expr2))
  | MkIf (expr1, expr2, expr3) ->
    if as_bool (eval expr1) then eval expr2 else eval expr3
;;
