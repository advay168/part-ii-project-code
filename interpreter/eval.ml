open! Base
open Language
module Effect = Stdlib.Effect
open Effect
open Effect.Deep

module Builtins = struct
  type t =
    | Fst
    | Snd
end

(* Mutually recursive modules as [Value.t] needs to depend on [Main.continuation]. *)
module rec Value : sig
  type t =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTup of t * t
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VFunBuiltin of Builtins.t
    | VContinuation of Main.kontinuation list

  val sexp_of_t : t -> Sexp.t
  val string_of_t : t -> string
end = struct
  type t =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTup of t * t
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VFunBuiltin of Builtins.t
    | VContinuation of Main.kontinuation list

  let rec string_of_t = function
    | VInt int -> Int.to_string int
    | VBool bool -> Bool.to_string bool
    | VUnit -> "()"
    | VTup (e1, e2) -> "(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
    | VFun _ -> "<fun>"
    | VContinuation _ -> "<kont>"
    | VFunBuiltin Fst -> "<builtin: fst>"
    | VFunBuiltin Snd -> "<builtin: snd>"
  ;;

  let sexp_of_t t = Sexp.Atom (string_of_t t)
end

and Main : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  type kontinuation = kontinuation' Ast.annotated
  and kontinuation'

  val eval : debug:bool -> Language.Ast.expr -> Value.t
end = struct
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  type expr_or_val =
    | Expr of Ast.expr
    | Value of Value.t
  [@@deriving sexp_of]

  let string_of_expr_or_val = function
    | Expr expr -> "E " ^ Pretty_print.pp expr
    | Value value -> "V " ^ Value.string_of_t value
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

  type kontinuation = kontinuation' Ast.annotated

  and kontinuation' =
    | CNot of hole
    | CBinOp1 of hole * Ast.binOp * Ast.expr * env
    | CBinOp2 of Value.t * Ast.binOp * hole
    | CIf of hole * Ast.expr * Ast.expr * env
    | CLet of Var.t * hole * Ast.expr * env
    | CLetRec of Var.t * hole * Ast.expr * env
    | CApply1 of hole * Ast.expr * env
    | CApply2 of Value.t * hole
    | CPerform of Var.t * hole * kontinuation list
    | CHandler of Ast.handler list * env
  [@@deriving sexp_of]

  type t =
    { c : expr_or_val
    ; e : env
    ; k : kontinuation list
    }

  type _ Effect.t += Breakpoint : t -> bool Effect.t

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

  let as_tuple = function
    | Value.VTup (v1, v2) -> v1, v2
    | v -> raise (TypeError ("tuple", v))
  ;;

  let perform_bin_op (v1, (op : Ast.binOp), v2) : Value.t =
    match op with
    | IAdd -> VInt (as_int v1 + as_int v2)
    | IMul -> VInt (as_int v1 * as_int v2)
    | IEql -> VBool (as_int v1 = as_int v2)
    | BAnd -> VBool (as_bool v1 && as_bool v2)
    | BOr -> VBool (as_bool v1 || as_bool v2)
    | EMkTuple -> VTup (v1, v2)
  ;;

  let perform_builtin (op : Builtins.t) (value : Value.t) : Value.t =
    match op with
    | Fst -> fst (as_tuple value)
    | Snd -> snd (as_tuple value)
  ;;

  let lookup name env =
    match Env.get name env with
    | Some v -> v
    | None -> raise (UnboundVarError (name, env))
  ;;

  let step ({ c = e_or_v; e = env; k = cs } : t) : t =
    let continue value (c : kontinuation) cs =
      let mk k = { c with x = k } in
      match c.x with
      | CNot Hole ->
        { c = Value (VBool (not (as_bool value))); e = env; k = cs }
      | CBinOp1 (Hole, op, expr, env) ->
        { c = Expr expr; e = env; k = mk (CBinOp2 (value, op, Hole)) :: cs }
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
        { c = Expr expr; e = env; k = mk (CApply2 (value, Hole)) :: cs }
      | CApply2 (v', Hole) ->
        (match v' with
         | VContinuation konts -> { c = Value value; e = env; k = konts @ cs }
         | VFunBuiltin builtin ->
           { c = Value (perform_builtin builtin value); e = env; k = cs }
         | _ ->
           let name, body, env = as_func v' in
           let env = force env in
           let env' = Env.set name value env in
           { c = Expr body; e = env'; k = cs })
      | CPerform (eff, Hole, saved_konts) ->
        (match cs with
         | [] -> raise (UnhandledEffect (eff, value))
         | ({ x = CHandler (handlers, env); _ } as deep_handler) :: cs
           when List.exists handlers ~f:(fun h -> String.equal h.eff eff) ->
           let handler =
             List.find_exn handlers ~f:(fun h -> String.equal h.eff eff)
           in
           let env' = Env.set handler.arg value env in
           let env' =
             Env.set
               handler.kont
               (Value.VContinuation (List.rev (deep_handler :: saved_konts)))
               env'
           in
           { c = Expr handler.body; e = env'; k = cs }
         | c :: cs ->
           { c = Value value
           ; e = env
           ; k = mk (CPerform (eff, Hole, c :: saved_konts)) :: cs
           })
      | CHandler _ -> { e = env; c = Value value; k = cs }
    in
    let translate_expr (e : Ast.expr) : t =
      let make ?k c =
        { c
        ; e = env
        ; k =
            (match k with
             | None -> cs
             | Some k -> { e with x = k } :: cs)
        }
      in
      match e.x with
      | MkInt int -> make @@ Value (VInt int)
      | MkBool bool -> make @@ Value (VBool bool)
      | MkUnit -> make @@ Value VUnit
      | MkBinOp (expr1, op, expr2) ->
        make ~k:(CBinOp1 (Hole, op, expr2, env)) @@ Expr expr1
      | MkNot expr -> make ~k:(CNot Hole) @@ Expr expr
      | MkIf (exprCond, exprTrue, exprFalse) ->
        make ~k:(CIf (Hole, exprTrue, exprFalse, env)) @@ Expr exprCond
      | MkVar name -> make @@ Value (lookup name env)
      | MkLet (name, expr1, expr2) -> begin
        match expr1.x with
        | MkFun _ -> make ~k:(CLetRec (name, Hole, expr2, env)) @@ Expr expr1
        | _ -> make ~k:(CLet (name, Hole, expr2, env)) @@ Expr expr1
      end
      | MkFun (name, body) -> make @@ Value (VFun (name, body, lazy env))
      | MkApply (exprFn, exprArg) ->
        make ~k:(CApply1 (Hole, exprArg, env)) @@ Expr exprFn
      | MkPerform (eff, expr) -> make ~k:(CPerform (eff, Hole, [])) @@ Expr expr
      | MkHandle (body, handler) ->
        make ~k:(CHandler (handler, env)) @@ Expr body
    in
    match e_or_v with
    | Value v -> begin
      match cs with
      | [] -> failwith "Internal error: No continuation to pass value to."
      | c :: cs -> continue v c cs
    end
    | Expr e -> translate_expr e
  ;;

  let builtins_env =
    Env.empty
    |> Env.set ~hidden:true "fst" (Value.VFunBuiltin Fst)
    |> Env.set ~hidden:true "snd" (Value.VFunBuiltin Snd)
  ;;

  let stringify { c; e; k } =
    let stringify_k k =
      [ "[" ]
      @ (k
         |> List.map ~f:(fun cont ->
           cont
           |> sexp_of_kontinuation
           |> Sexp.to_string_hum
           |> String.tr ~target:'\n' ~replacement:' '
           |> fun x -> x ^ ","))
      @ [ "]" ]
    in
    Language.Ast.without_showing_anns (fun () ->
      [ string_of_expr_or_val c ], [ string_of_env e ], stringify_k k)
  ;;

  let rec driver ~history ~break_immediately (state : t) =
    let break_immediately =
      if break_immediately then perform (Breakpoint state) else false
    in
    let history = state :: history in
    match state with
    | { c = Value v; e = _; k = [] } -> v, history
    | { c = Expr { breakpoint = true; _ }; _ } ->
      Stdio.print_endline "Breakpoint hit";
      let break_immediately = perform (Breakpoint state) in
      driver ~history ~break_immediately (step state)
    | _ -> driver ~history ~break_immediately (step state)
  ;;

  module Debugger = struct
    type state =
      | Stepping of
          { cek : t
          ; driver_k : (bool, Value.t * t list) continuation
          ; full_expr : Ast.expr
          }

    let prompt () =
      Stdio.print_string "debugger> ";
      Debugger_cmd_parser.parse (Stdio.In_channel.input_line_exn Stdio.stdin)
    ;;

    let rec debugger state =
      match state with
      | Stepping { cek; driver_k; full_expr } -> begin
        match prompt () with
        | Nop -> debugger state
        | Help ->
          Stdio.print_string Debugger_cmd_parser.help_text;
          debugger state
        | Continue -> continue driver_k false
        | Step -> continue driver_k true
        | ShowState ->
          Util.print_table ~header:("C", "E", "K") ~stringify [ cek ];
          debugger state
        | Where ->
          let () =
            match cek.c with
            | Value _ -> Stdio.print_endline "Cannot show location of value."
            | Expr expr ->
              let (sl, sc), (el, ec) = Ast.linecol_of_span expr.span in
              Printf.sprintf "%d:%d..%d:%d" sl sc el ec |> Stdio.print_endline
          in
          debugger state
        | Breakpoint pos ->
          let success = Ast.mark_breakpoint pos full_expr in
          if success
          then Stdio.print_endline "Breakpoint set."
          else Stdio.print_endline "Could not set breakpoint.";
          debugger state
        | Inspect var -> begin
          match Env.get var cek.e with
          | None ->
            Stdio.print_endline "Variable not present";
            debugger state
          | Some value ->
            value
            |> Value.string_of_t
            |> Printf.sprintf "`%s`: %s" var
            |> Stdio.print_endline;
            debugger state
        end
      end
    ;;
  end

  let eval ~debug expr =
    let cek = { c = Expr expr; e = builtins_env; k = [] } in
    let evaluated_value, history =
      match driver ~history:[] ~break_immediately:debug cek with
      | v -> v
      | effect Breakpoint cek, driver_k ->
        Util.print_table ~header:("C", "E", "K") ~stringify [ cek ];
        Debugger.debugger (Stepping { cek; driver_k; full_expr = expr })
    in
    if debug
    then (
      let history = List.rev history in
      Util.print_table ~header:("C", "E", "K") ~stringify history);
    evaluated_value
  ;;
end
