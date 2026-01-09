open! Base

let wrap s = "(" ^ s ^ ")"

let rec pp (expr : Ast.expr) =
  (match expr.x with
   | MkInt int -> Int.to_string int
   | MkBool bool -> Bool.to_string bool
   | MkUnit -> "()"
   | MkBinOp (e1, IAdd, e2) -> pp e1 ^ " + " ^ pp e2
   | MkBinOp (e1, IMul, e2) -> pp e1 ^ " * " ^ pp e2
   | MkBinOp (e1, IEql, e2) -> pp e1 ^ " = " ^ pp e2
   | MkBinOp (e1, BAnd, e2) -> pp e1 ^ " && " ^ pp e2
   | MkBinOp (e1, BOr, e2) -> pp e1 ^ " || " ^ pp e2
   | MkBinOp (e1, EMkTuple, e2) -> "(" ^ pp e1 ^ ", " ^ pp e2 ^ ")"
   | MkNot e -> "~" ^ pp e
   | MkIf (e1, e2, e3) ->
     "if " ^ pp e1 ^ " then " ^ pp e2 ^ " else " ^ pp e3 ^ " end"
   | MkVar name -> name
   | MkLet (name, e1, e2) ->
     "let " ^ name ^ " := " ^ pp e1 ^ " in " ^ pp e2 ^ " end"
   | MkFun (name, e) -> "fun " ^ name ^ " -> " ^ pp e ^ " end"
   | MkApply (e1, e2) -> pp e1 ^ " @ " ^ pp e2
   | MkPerform (eff, e) -> "perform (" ^ eff ^ " " ^ pp e ^ ")"
   | MkHandle (e, hs) ->
     "handle "
     ^ pp e
     ^ " with "
     ^ (hs |> List.map ~f:pp_handler |> String.concat ~sep:"\n")
     ^ " end")
  |> wrap

and pp_handler { eff; arg; kont; body } =
  "| " ^ eff ^ ", " ^ arg ^ ", " ^ kont ^ " -> " ^ pp body
;;
