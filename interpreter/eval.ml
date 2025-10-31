open! Base

let rec eval (expr : Parser.Ast.expr) =
  match expr.e with
  | Parser.Ast.MkInt int -> int
  | MkAdd (expr1, expr2) -> eval expr1 + eval expr2
  | MkMult (expr1, expr2) -> eval expr1 * eval expr2
;;
