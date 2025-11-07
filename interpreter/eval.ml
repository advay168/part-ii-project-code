open! Base

exception TypeError of string * Value.t

let as_int = function
  | Value.VInt int -> int
  | v -> raise (TypeError ("expected int", v))
;;

let as_bool = function
  | Value.VBool bool -> bool
  | v -> raise (TypeError ("expected bool", v))
;;

let as_func = function
  | Value.VFun (name, body, store) -> name, body, store
  | v -> raise (TypeError ("expected func", v))
;;

let equal (v1, v2) = as_int v1 = as_int v2

let rec eval (store, (expr : Language.Ast.expr)) : Value.t =
  match expr.e with
  | MkInt int -> VInt int
  | MkAdd (expr1, expr2) ->
    VInt (as_int (eval (store, expr1)) + as_int (eval (store, expr2)))
  | MkMult (expr1, expr2) ->
    VInt (as_int (eval (store, expr1)) * as_int (eval (store, expr2)))
  | MkBool bool -> VBool bool
  | MkAnd (expr1, expr2) ->
    VBool (as_bool (eval (store, expr1)) && as_bool (eval (store, expr2)))
  | MkOr (expr1, expr2) ->
    VBool (as_bool (eval (store, expr1)) || as_bool (eval (store, expr2)))
  | MkNot expr -> VBool (not (as_bool (eval (store, expr))))
  | MkEqual (expr1, expr2) ->
    VBool (equal (eval (store, expr1), eval (store, expr2)))
  | MkIf (expr1, expr2, expr3) ->
    if as_bool (eval (store, expr1))
    then eval (store, expr2)
    else eval (store, expr3)
  | MkVar name -> Store.get name store |> Option.value_exn
  | MkLet (name, expr1, expr2) ->
    let value = eval (store, expr1) in
    let value =
      match value with
      | VFun (var, expr, store) ->
        (* Add binding to function value to allow recursion. *)
        let rec f =
          Value.VFun (var, expr, lazy (Store.set name f (force store)))
        in
        f
      | v -> v
    in
    let store' = Store.set name value store in
    eval (store', expr2)
  | MkFun (name, expr) -> VFun (name, expr, lazy store)
  | MkApply (expr1, expr2) ->
    let name, body, store' = as_func (eval (store, expr1)) in
    let store' = force store' in
    let arg = eval (store, expr2) in
    eval (Store.set name arg store', body)
;;
