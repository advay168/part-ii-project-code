open! Base
open Language
module Effect = Stdlib.Effect
open Effect
open Effect.Deep

(* Mutually recursive modules as [Value.t] needs to depend on [Main.continuation]. *)
module rec Value : sig
  type t =
    | VInt of int
    | VBool of bool
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VContinuation of Main.continuation list

  val sexp_of_t : t -> Sexp.t
  val string_of_t : t -> string
end = struct
  type t =
    | VInt of int
    | VBool of bool
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VContinuation of Main.continuation list

  let string_of_t = function
    | VInt int -> Int.to_string int
    | VBool bool -> Bool.to_string bool
    | VFun _ -> "<fun>"
    | VContinuation _ -> "<kont>"
  ;;

  let sexp_of_t t = Sexp.Atom (string_of_t t)
end

and Main : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception LangException of Value.t

  type continuation

  val eval : debug:bool -> Language.Ast.expr -> Value.t
end = struct
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception LangException of Value.t

  type expr_or_val =
    | Expr of Ast.expr
    | Value of Value.t
  [@@deriving sexp_of]

  let string_of_expr_or_val = function
    | Expr expr -> Pretty_print.pp expr
    | Value value -> Value.string_of_t value
  ;;

  (** Solely for readability to denote where the continuation expects a hole.  *)
  type hole = Hole [@@deriving sexp_of]

  (** Override [Var] printing to make it easier to read in debugging. *)
  module Var = struct
    include Var

    let sexp_of_t t = Sexp.Atom ("'" ^ t ^ "'")
  end

  type env = Value.t Env.t

  let string_of_env = Env.string_of_t Value.string_of_t
  let sexp_of_env env = env |> string_of_env |> Sexp.Atom

  type continuation =
    | CNot of hole
    | CBinOp1 of hole * Ast.binOp * Ast.expr * env
    | CBinOp2 of Value.t * Ast.binOp * hole
    | CIf of hole * Ast.expr * Ast.expr * env
    | CLet of Var.t * hole * Ast.expr * env
    | CLetRec of Var.t * hole * Ast.expr * env
    | CApply1 of hole * Ast.expr * env
    | CApply2 of Value.t * hole
    | CPerform of hole * continuation list
    | CHandler of Var.t * Var.t * Ast.expr * env
  [@@deriving sexp_of]

  type t =
    { c : expr_or_val
    ; e : env
    ; k : continuation list
    }

  type _ Effect.t += Debugger : t -> t Effect.t

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
        (match v' with
         | VContinuation konts -> { c = Value value; e = env; k = konts @ cs }
         | _ ->
           let name, body, env = as_func v' in
           let env = force env in
           let env' = Env.set name value env in
           { c = Expr body; e = env'; k = cs })
      | CPerform (Hole, saved_konts) ->
        (match cs with
         | [] -> raise (LangException value)
         | (CHandler (name, kont, body, env) as deep_handler) :: cs ->
           let env' = Env.set name value env in
           let env' =
             Env.set
               kont
               (Value.VContinuation (saved_konts @ [ deep_handler ]))
               env'
           in
           { c = Expr body; e = env'; k = cs }
         | c :: cs ->
           { c = Value value
           ; e = env
           ; k = CPerform (Hole, c :: saved_konts) :: cs
           })
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
      | MkPerform expr ->
        { c = Expr expr; e = env; k = CPerform (Hole, []) :: cs }
      | MkHandle (body, name, kont, handler) ->
        { c = Expr body
        ; e = env
        ; k = CHandler (name, kont, handler, env) :: cs
        }
    in
    match e_or_v with
    | Value v ->
      (match cs with
       | [] -> failwith "Internal error: No continuation to pass value to."
       | c :: cs -> continue v c cs)
    | Expr { e; loc = _ } -> translate_expr e
  ;;

  let stringify { c; e; k } =
    let stringify_k k =
      [ "[" ]
      @ (k
         |> List.map ~f:(fun cont ->
           cont
           |> sexp_of_continuation
           |> Sexp.to_string_hum
           |> String.tr ~target:'\n' ~replacement:' '
           |> fun x -> x ^ ","))
      @ [ "]" ]
    in
    Language.Ast.without_showing_locs (fun () ->
      [ string_of_expr_or_val c ], [ string_of_env e ], stringify_k k)
  ;;

  let nop_debugger f =
    match f () with
    | output -> output
    | effect Debugger cek_state, k -> continue k cek_state
  ;;

  let repl_debugger f =
    match f () with
    | output -> output
    | effect Debugger cek_state, k ->
      let s1, s2, s3 = stringify cek_state in
      Stdio.print_string "C: ";
      List.iter ~f:Stdio.print_endline s1;
      Stdio.print_string "E: ";
      List.iter ~f:Stdio.print_endline s2;
      Stdio.print_string "K: ";
      List.iter ~f:Stdio.print_endline s3;
      Stdio.print_string "debug> ";
      Out_channel.flush_all ();
      let _inp = Stdio.In_channel.input_line_exn Stdio.stdin in
      continue k cek_state
  ;;

  let eval ~debug expr =
    let rec driver ~history (state : t) =
      let history = state :: history in
      match state with
      | { c = Value v; e = _; k = [] } -> v, history
      | _ ->
        let state' = perform (Debugger state) in
        driver ~history (step state')
    in
    let d = if debug then repl_debugger else nop_debugger in
    let evaluated_value, history =
      d (fun () -> driver ~history:[] { c = Expr expr; e = Env.empty; k = [] })
    in
    if debug
    then (
      let history = List.rev history in
      Util.print_table ~header:("C", "E", "K") ~stringify history);
    evaluated_value
  ;;
end
