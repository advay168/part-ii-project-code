open! Base

exception TypeError of string * Value.t
exception UnboundVarError of string * Value.t Env.t
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
  | Value.VFun (name, body, env) -> name, body, env
  | v -> raise (TypeError ("func", v))
;;

let perform_bin_op (v1, (op : Language.Ast.binOp), v2) : Value.t =
  match op with
  | IAdd -> VInt (as_int v1 + as_int v2)
  | IMul -> VInt (as_int v1 * as_int v2)
  | IEql -> VBool (as_int v1 = as_int v2)
  | BAnd -> VBool (as_bool v1 && as_bool v2)
  | BOr -> VBool (as_bool v1 || as_bool v2)
;;

let lookup name env =
  match Env.get name env with
  | Some v -> v
  | None -> raise (UnboundVarError (name, env))
;;

let eval expr =
  let rec eval (env, (expr : Language.Ast.expr)) : Value.t =
    match expr.e with
    | MkInt int -> VInt int
    | MkBinOp (expr1, op, expr2) ->
      perform_bin_op (eval (env, expr1), op, eval (env, expr2))
    | MkBool bool -> VBool bool
    | MkNot expr -> VBool (not (as_bool (eval (env, expr))))
    | MkIf (cond, exprTrue, exprFalse) ->
      if as_bool (eval (env, cond))
      then eval (env, exprTrue)
      else eval (env, exprFalse)
    | MkVar name -> lookup name env
    | MkLet (name, expr1, expr2) ->
      let value =
        match expr1.e with
        | MkFun (var, body) ->
          let rec f = Value.VFun (var, body, lazy (Env.set name f env)) in
          f
        | _ -> eval (env, expr1)
      in
      let env' = Env.set name value env in
      eval (env', expr2)
    | MkFun (name, body) -> VFun (name, body, lazy env)
    | MkApply (exprFn, exprArg) ->
      let name, body, env' = as_func (eval (env, exprFn)) in
      let env' = force env' in
      let arg = eval (env, exprArg) in
      eval (Env.set name arg env', body)
    | MkRaise expr -> raise (LangException (eval (env, expr)))
    | MkTry (body, name, handler) ->
      (try eval (env, body) with
       | LangException exn -> eval (Env.set name exn env, handler))
  in
  eval (Env.empty, expr)
;;
