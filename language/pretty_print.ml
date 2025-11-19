open! Base

let wrap s = "(" ^ s ^ ")"

let rec pp (expr : Ast.expr) =
  (match expr.e with
   | MkInt int -> Int.to_string int
   | MkAdd (e1, e2) -> pp e1 ^ " + " ^ pp e2
   | MkMult (e1, e2) -> pp e1 ^ " * " ^ pp e2
   | MkBool bool -> Bool.to_string bool
   | MkAnd (e1, e2) -> pp e1 ^ " && " ^ pp e2
   | MkOr (e1, e2) -> pp e1 ^ " || " ^ pp e2
   | MkNot e -> "~" ^ pp e
   | MkEqual (e1, e2) -> pp e1 ^ " = " ^ pp e2
   | MkIf (e1, e2, e3) ->
     "if " ^ pp e1 ^ " then " ^ pp e2 ^ " else " ^ pp e3 ^ " endif"
   | MkVar name -> name
   | MkLet (name, e1, e2) ->
     "let " ^ name ^ " := " ^ pp e1 ^ " in " ^ pp e2 ^ " endlet"
   | MkFun (name, e) -> "fun " ^ name ^ " -> " ^ pp e ^ " endfun"
   | MkApply (e1, e2) -> pp e1 ^ " @ " ^ pp e2
   | MkRaise e -> "raise ( " ^ pp e ^ " )"
   | MkTry (e1, name, e2) ->
     "try " ^ pp e1 ^ " with " ^ name ^ " -> " ^ pp e2 ^ " endtry")
  |> wrap
;;
