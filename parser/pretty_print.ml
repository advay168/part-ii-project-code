open! Base

let rec pp = function
  | Ast.MkInt int -> Int.to_string int
  | MkAdd (e1, e2) -> "(" ^ pp e1 ^ "+" ^ pp e2 ^ ")"
  | MkMult (e1, e2) -> "(" ^ pp e1 ^ "*" ^ pp e2 ^ ")"
;;
