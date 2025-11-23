open! Base
open Language

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

let perform_bin_op (v1, (op : Ast.binOp), v2) : Value.t =
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

module CEK = struct
  type env = Value.t Env.t

  type expr_or_val =
    | Expr of Ast.expr
    | Value of Value.t

  (** Solely for readability to denote where the continuation expects a hole.  *)
  type hole = Hole

  type continuation =
    | CNot of hole
    | CBinOp1 of hole * Ast.binOp * Ast.expr * env
    | CBinOp2 of Value.t * Ast.binOp * hole
    | CIf of hole * Ast.expr * Ast.expr * env
    | CLet of Var.t * hole * Ast.expr * env
    | CLetRec of Var.t * hole * Ast.expr * env
    | CApply1 of hole * Ast.expr * env
    | CApply2 of Value.t * hole
    | CRaise of hole
    | CHandler of Var.t * Ast.expr * env

  type t =
    { c : expr_or_val
    ; e : env
    ; k : continuation list
    }

  let step ({ e = env; c = e_or_v; k = cs } : t) : t =
    let continue value c cs =
      match c with
      | CNot Hole ->
        { c = Value (VBool (not (as_bool value))); e = env; k = cs }
      | CBinOp1 (Hole, op, expr, env) ->
        { c = Expr expr; e = env; k = CBinOp2 (value, op, Hole) :: cs }
      | CBinOp2 (v', op, Hole) ->
        let result = perform_bin_op (v', op, value) in
        { c = Value result; e = env; k = cs }
      | CIf (Hole, exprTrue, exprFalse, env) ->
        { c = Expr (if as_bool value then exprTrue else exprFalse)
        ; e = env
        ; k = cs
        }
      | CLet (name, Hole, expr, env) ->
        let env' = Env.set name value env in
        { c = Expr expr; e = env'; k = cs }
      | CLetRec (name, Hole, expr, env) ->
        (* Only functions can be recursive *)
        let arg_name, body, env' = as_func value in
        let rec f =
          Value.VFun (arg_name, body, lazy (Env.set name f (force env')))
        in
        let env' = Env.set name f env in
        { c = Expr expr; e = env'; k = cs }
      | CApply1 (Hole, expr, env) ->
        { c = Expr expr; e = env; k = CApply2 (value, Hole) :: cs }
      | CApply2 (v', Hole) ->
        let name, body, env = as_func v' in
        let env = force env in
        let env' = Env.set name value env in
        { c = Expr body; e = env'; k = cs }
      | CRaise Hole ->
        (match cs with
         | [] -> raise (LangException value)
         | CHandler (name, body, env) :: cs ->
           let env' = Env.set name value env in
           { c = Expr body; e = env'; k = cs }
         | _ :: cs -> { c = Value value; e = env; k = CRaise Hole :: cs })
      | CHandler _ -> { e = env; c = Value value; k = cs }
    in
    let translate_expr = function
      | Ast.MkInt int -> { c = Value (VInt int); e = env; k = cs }
      | MkBinOp (expr1, op, expr2) ->
        { e = env; c = Expr expr1; k = CBinOp1 (Hole, op, expr2, env) :: cs }
      | MkBool bool -> { c = Value (VBool bool); e = env; k = cs }
      | MkNot expr -> { c = Expr expr; e = env; k = CNot Hole :: cs }
      | MkIf (exprCond, exprTrue, exprFalse) ->
        { c = Expr exprCond
        ; e = env
        ; k = CIf (Hole, exprTrue, exprFalse, env) :: cs
        }
      | MkVar name -> { c = Value (lookup name env); e = env; k = cs }
      | MkLet (name, expr1, expr2) ->
        (match expr1.e with
         | MkFun _ ->
           { c = Expr expr1
           ; e = env
           ; k = CLetRec (name, Hole, expr2, env) :: cs
           }
         | _ ->
           { c = Expr expr1; e = env; k = CLet (name, Hole, expr2, env) :: cs })
      | MkFun (name, body) ->
        { c = Value (VFun (name, body, lazy env)); e = env; k = cs }
      | MkApply (exprFn, exprArg) ->
        { c = Expr exprFn; e = env; k = CApply1 (Hole, exprArg, env) :: cs }
      | MkRaise expr -> { c = Expr expr; e = env; k = CRaise Hole :: cs }
      | MkTry (body, name, handler) ->
        { c = Expr body; e = env; k = CHandler (name, handler, env) :: cs }
    in
    match e_or_v with
    | Value v ->
      (match cs with
       | [] -> failwith "Internal error: No continuation to pass value to."
       | c :: cs -> continue v c cs)
    | Expr { e; loc = _ } -> translate_expr e
  ;;

  let eval expr =
    let rec driver (state : t) =
      match state with
      | { c = Value v; e = _; k = [] } -> v
      | _ -> driver (step state)
    in
    driver { c = Expr expr; e = Env.empty; k = [] }
  ;;
end

let cek_eval = CEK.eval

let eval expr =
  let rec eval (env, (expr : Ast.expr)) : Value.t =
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
