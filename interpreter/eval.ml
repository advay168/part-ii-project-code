open! Base

exception TypeError of string * Value.t
exception UnboundVarError of string * Value.t Store.t
exception LangException of Value.t

let as_int = function
  | Value.VInt int -> int
  | v -> raise (TypeError ("int", v))
;;

let as_bool = function
  | Value.VBool bool -> bool
  | v -> raise (TypeError ("bool", v))
;;

let as_func = function
  | Value.VFun (name, body, store) -> name, body, store
  | v -> raise (TypeError ("func", v))
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
  | MkIf (cond, exprTrue, exprFalse) ->
    if as_bool (eval (store, cond))
    then eval (store, exprTrue)
    else eval (store, exprFalse)
  | MkVar name ->
    (match Store.get name store with
     | Some x -> x
     | None -> raise (UnboundVarError (name, store)))
  | MkLet (name, expr1, expr2) ->
    let value =
      match expr1.e with
      | MkFun (var, body) ->
        let rec f = Value.VFun (var, body, lazy (Store.set name f store)) in
        f
      | _ -> eval (store, expr1)
    in
    let store' = Store.set name value store in
    eval (store', expr2)
  | MkFun (name, body) -> VFun (name, body, lazy store)
  | MkApply (exprFn, exprArg) ->
    let name, body, store' = as_func (eval (store, exprFn)) in
    let store' = force store' in
    let arg = eval (store, exprArg) in
    eval (Store.set name arg store', body)
  | MkRaise expr -> raise (LangException (eval (store, expr)))
  | MkTry (body, name, handler) ->
    (try eval (store, body) with
     | LangException exn -> eval (Store.set name exn store, handler))
;;
