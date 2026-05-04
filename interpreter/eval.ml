open! Base
open Language

module Builtins = struct
  type t =
    | Fst
    | Snd
end

(* Mutually recursive modules as [Value.t] depends on [Eval.continuation]. *)
module rec Value : sig
  type t =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTup of t * t
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VFunBuiltin of Builtins.t
    | VContinuation of Eval.kontinuation list

  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string

  (* val to_int : t -> int *)
  val of_int : int -> t

  (** Raises if not a bool. *)
  val to_bool : t -> bool

  val of_bool : bool -> t

  (** Raises if not a func. *)
  val to_func : t -> Var.t * Ast.expr * t Env.t lazy_t

  val of_func : arg_name:Var.t -> body:Ast.expr -> env:t Env.t lazy_t -> t

  (** Raises if types not appropriate. *)
  val perform_bin_op : t * Ast.binOp * t -> t

  (** Raises if types not appropriate. *)
  val perform_builtin : Builtins.t -> t -> t
end = struct
  type t =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTup of t * t
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VFunBuiltin of Builtins.t
    | VContinuation of Eval.kontinuation list

  let rec to_string = function
    | VInt int -> Int.to_string int
    | VBool bool -> Bool.to_string bool
    | VUnit -> "()"
    | VTup (e1, e2) -> "(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ")"
    | VFun _ -> "<fun>"
    | VContinuation _ -> "<kont>"
    | VFunBuiltin Fst -> "<builtin: fst>"
    | VFunBuiltin Snd -> "<builtin: snd>"
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let to_int = function
    | Value.VInt int -> int
    | v -> raise (Eval.TypeError ("int", v))
  ;;

  let of_int n = Value.VInt n

  let to_bool = function
    | Value.VBool bool -> bool
    | v -> raise (Eval.TypeError ("bool", v))
  ;;

  let of_bool b = Value.VBool b

  let to_func = function
    | Value.VFun (name, body, env) -> name, body, env
    | v -> raise (Eval.TypeError ("func", v))
  ;;

  let of_func ~arg_name ~body ~env = VFun (arg_name, body, env)

  let as_tuple = function
    | Value.VTup (v1, v2) -> v1, v2
    | v -> raise (Eval.TypeError ("tuple", v))
  ;;

  let perform_bin_op (v1, (op : Ast.binOp), v2) : Value.t =
    match op with
    | IAdd -> VInt (to_int v1 + to_int v2)
    | IMul -> VInt (to_int v1 * to_int v2)
    | IEql -> VBool (to_int v1 = to_int v2)
    | BAnd -> VBool (to_bool v1 && to_bool v2)
    | BOr -> VBool (to_bool v1 || to_bool v2)
    | EMkTuple -> VTup (v1, v2)
  ;;

  let perform_builtin (op : Builtins.t) (value : Value.t) : Value.t =
    match op with
    | Fst -> fst (as_tuple value)
    | Snd -> snd (as_tuple value)
  ;;
end

and Eval : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of Var.t * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  (* type kontinuation = kontinuation' Ast.annotated *)
  (* and kontinuation' *)

  val eval : source:string -> Language.Ast.expr -> Value.t

  (* For Debugger. *)
  type expr_or_val =
    | CtrlExpr of Ast.expr
    | CtrlValue of Value.t
  [@@deriving sexp_of]

  type hole = Hole
  type env = Value.t Env.t

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
    | CIOHandler

  type cek =
    { c : expr_or_val
    ; e : env
    ; k : kontinuation list
    ; state_idx : int
    }

  exception Breakpoint of cek list * cek

  (** [times] refers to the number of times to run before breakpointing. 
      [ignore_bp] means to not trigger any AST breakpoints on this step. Useful
      to continue execution from the debugger.
   *)
  val driver
    :  ?ignore_bp:bool
    -> ?times:int
    -> ?history:cek list
    -> cek
    -> Value.t * cek list option

  val initial_state : Ast.expr -> cek
end = struct
  exception TypeError of string * Value.t
  exception UnboundVarError of Var.t * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  type expr_or_val =
    | CtrlExpr of Ast.expr
    | CtrlValue of Value.t
  [@@deriving sexp_of]

  (** Solely for readability to denote where the continuation expects a hole.  *)
  type hole = Hole [@@deriving sexp_of]

  type env = Value.t Env.t

  let string_of_env = Env.string_of_t Value.to_string
  let sexp_of_env env = env |> string_of_env |> Sexp.Atom

  type kontinuation = kontinuation' Ast.annotated

  and kontinuation' =
    | CNot of hole
    | CBinOp1 of hole * Ast.binOp * Ast.expr * env
    | CBinOp2 of Value.t * Ast.binOp * hole
    | CIf of hole * Ast.expr * Ast.expr * env
    | CLet of Language.Var.t * hole * Ast.expr * env
    | CLetRec of Language.Var.t * hole * Ast.expr * env
    | CApply1 of hole * Ast.expr * env
    | CApply2 of Value.t * hole
    | CPerform of Language.Var.t * hole * kontinuation list
    | CHandler of Ast.handler list * env
    | CIOHandler
  [@@deriving sexp_of]

  type cek =
    { c : expr_or_val
    ; e : env
    ; k : kontinuation list
    ; state_idx : int
    }

  let lookup name env =
    match Env.get name env with
    | Some v -> v
    | None -> raise (UnboundVarError (name, env))
  ;;

  let io_handlers =
    let print_handler =
      ( Var.make "Print"
      , fun value ->
          Value.to_string value |> Stdio.print_endline;
          Value.VUnit )
    in
    let input_handler =
      ( Var.make "Input"
      , fun _value ->
          let input = Stdio.In_channel.input_line_exn Stdio.stdin in
          let integer = Int.of_string input in
          Value.VInt integer )
    in
    [ print_handler; input_handler ]
  ;;

  let step ({ c = e_or_v; e = env; k = cs; state_idx } : cek) : cek =
    let state_idx = state_idx + 1 in
    let continue value (c : kontinuation) cs =
      let mk k = { c with x = k; breakpoint = false } in
      match c.x with
      | CNot Hole ->
        { c = CtrlValue (Value.of_bool (not (Value.to_bool value)))
        ; e = env
        ; k = cs
        ; state_idx
        }
      | CBinOp1 (Hole, op, expr, env) ->
        { c = CtrlExpr expr
        ; e = env
        ; k = mk (CBinOp2 (value, op, Hole)) :: cs
        ; state_idx
        }
      | CBinOp2 (v', op, Hole) ->
        let result = Value.perform_bin_op (v', op, value) in
        { c = CtrlValue result; e = env; k = cs; state_idx }
      | CIf (Hole, exprTrue, exprFalse, env) ->
        { c = CtrlExpr (if Value.to_bool value then exprTrue else exprFalse)
        ; e = env
        ; k = cs
        ; state_idx
        }
      | CLet (name, Hole, expr, env) ->
        let env' = Env.set name value env in
        { c = CtrlExpr expr; e = env'; k = cs; state_idx }
      | CLetRec (name, Hole, expr, env) ->
        (* Only functions can be recursive *)
        let arg_name, body, env' = Value.to_func value in
        let rec f =
          Value.VFun (arg_name, body, lazy (Env.set name f (force env')))
        in
        let env' = Env.set name f env in
        { c = CtrlExpr expr; e = env'; k = cs; state_idx }
      | CApply1 (Hole, expr, env) ->
        { c = CtrlExpr expr
        ; e = env
        ; k = mk (CApply2 (value, Hole)) :: cs
        ; state_idx
        }
      | CApply2 (v', Hole) -> begin
        match v' with
        | VContinuation konts ->
          { c = CtrlValue value; e = env; k = konts @ cs; state_idx }
        | VFunBuiltin builtin ->
          { c = CtrlValue (Value.perform_builtin builtin value)
          ; e = env
          ; k = cs
          ; state_idx
          }
        | _ ->
          let name, body, env = Value.to_func v' in
          let env = force env in
          let env' = Env.set name value env in
          { c = CtrlExpr body; e = env'; k = cs; state_idx }
      end
      | CPerform (eff, Hole, saved_konts) -> begin
        match cs with
        | [] -> raise (UnhandledEffect (eff, value))
        | { x = CIOHandler; _ } :: _
          when List.exists io_handlers ~f:(fun (name, _handler) ->
                 Var.equal name eff) ->
          let _name, handler =
            List.find_exn io_handlers ~f:(fun (name, _handler) ->
              Var.equal name eff)
          in
          { c = CtrlValue (handler value)
          ; e = env
          ; k = List.rev saved_konts @ cs
          ; state_idx
          }
        | ({ x = CHandler (handlers, env); _ } as deep_handler) :: cs
          when List.exists handlers ~f:(fun h -> Var.equal h.eff eff) ->
          let handler =
            List.find_exn handlers ~f:(fun h -> Var.equal h.eff eff)
          in
          let env' = Env.set handler.arg value env in
          let env' =
            Env.set
              handler.kont
              (Value.VContinuation (List.rev (deep_handler :: saved_konts)))
              env'
          in
          { c = CtrlExpr handler.body; e = env'; k = cs; state_idx }
        | c :: cs ->
          { c = CtrlValue value
          ; e = env
          ; k = mk (CPerform (eff, Hole, c :: saved_konts)) :: cs
          ; state_idx
          }
      end
      | CHandler _ -> { e = env; c = CtrlValue value; k = cs; state_idx }
      | CIOHandler -> { e = env; c = CtrlValue value; k = cs; state_idx }
    in
    let translate_expr (e : Ast.expr) : cek =
      let make ?k c =
        let k =
          match k with
          | None -> cs
          | Some k -> { e with x = k; breakpoint = false } :: cs
        in
        { c; e = env; k; state_idx }
      in
      let make_ctrl_value v = make (CtrlValue v) in
      match e.x with
      | MkInt int -> make_ctrl_value @@ Value.of_int int
      | MkBool bool -> make_ctrl_value @@ Value.of_bool bool
      | MkUnit -> make_ctrl_value VUnit
      | MkBinOp (expr1, op, expr2) ->
        make ~k:(CBinOp1 (Hole, op, expr2, env)) @@ CtrlExpr expr1
      | MkNot expr -> make ~k:(CNot Hole) @@ CtrlExpr expr
      | MkIf (exprCond, exprTrue, exprFalse) ->
        make ~k:(CIf (Hole, exprTrue, exprFalse, env)) @@ CtrlExpr exprCond
      | MkVar name -> make_ctrl_value (lookup name env)
      | MkLet (name, expr1, expr2) -> begin
        match expr1.x with
        | MkFun _ ->
          make ~k:(CLetRec (name, Hole, expr2, env)) @@ CtrlExpr expr1
        | _ -> make ~k:(CLet (name, Hole, expr2, env)) @@ CtrlExpr expr1
      end
      | MkFun (arg_name, body) ->
        make_ctrl_value @@ Value.of_func ~arg_name ~body ~env:(lazy env)
      | MkApply (exprFn, exprArg) ->
        make ~k:(CApply1 (Hole, exprArg, env)) @@ CtrlExpr exprFn
      | MkPerform (eff, expr) ->
        make ~k:(CPerform (eff, Hole, [])) @@ CtrlExpr expr
      | MkHandle (body, handler) ->
        make ~k:(CHandler (handler, env)) @@ CtrlExpr body
    in
    let next_state =
      match e_or_v with
      | CtrlValue v -> begin
        match cs with
        | [] -> failwith "Internal error: No continuation to pass value to."
        | c :: cs -> continue v c cs
      end
      | CtrlExpr e -> translate_expr e
    in
    (* Verify that no matter which reduction rule applied, the state_idx is as
       expected. *)
    assert (next_state.state_idx = state_idx);
    next_state
  ;;

  type history = cek list

  exception Breakpoint of history * cek

  (** [times] refers to the number of times to run before breakpointing. 
      [ignore_bp] means to not trigger any AST breakpoints on this step. Useful
      to continue execution from the debugger.
   *)
  let rec driver ?(ignore_bp = false) ?times ?history:old_history state =
    let times =
      match times with
      | Some 0 when not ignore_bp ->
        raise (Breakpoint (Option.value_exn old_history, state))
      | Some n -> Some (n - 1)
      | None -> None
    in
    let history =
      Option.map ~f:(fun old_history -> state :: old_history) old_history
    in
    match state with
    | { c = CtrlValue v; e = _; k = []; state_idx = _ } -> v, history
    | { c = CtrlValue _
      ; e = _
      ; k = ({ breakpoint = true; _ } as kk) :: _
      ; state_idx = _
      } ->
      kk.breakpoint <- false;
      raise (Breakpoint (Option.value_exn old_history, state))
    | { c = CtrlExpr { breakpoint = true; _ }; _ } when not ignore_bp ->
      Stdio.print_endline "Breakpoint hit";
      raise (Breakpoint (Option.value_exn old_history, state))
    | _ -> driver ?history ?times (step state)
  ;;

  let builtins_env =
    Env.empty
    |> Env.set ~hidden:true (Var.make "fst") (Value.VFunBuiltin Fst)
    |> Env.set ~hidden:true (Var.make "snd") (Value.VFunBuiltin Snd)
  ;;

  let initial_state expr =
    { c = CtrlExpr expr
    ; e = builtins_env
    ; k = [ Ast.make (Lexing.dummy_pos, Lexing.dummy_pos) CIOHandler ]
    ; state_idx = 0
    }
  ;;

  let eval ~source:_ expr =
    let cek = initial_state expr in
    let value, _ = driver cek in
    value
  ;;
end

and Debugger : sig
  val eval
    :  ?break_at_start:bool
    -> source:string
    -> Language.Ast.expr
    -> Value.t * Eval.cek list

  (* val print_cek : source:string -> Eval.cek -> unit *)
end = struct
  open Eval

  type dbg_state =
    { history : cek list
    ; current_cek : cek
    ; full_expr : Ast.expr
    }

  let prompt () =
    Stdio.print_string "debugger> ";
    Debugger_cmd_parser.parse (Stdio.In_channel.input_line_exn Stdio.stdin)
  ;;

  let string_of_env = Env.string_of_t Value.to_string

  let rec stringify_kont ~source (k : kontinuation) =
    let module Expr =
      Ast.Make_to_string (struct
        let source = source
      end)
    in
    let hole = "⬤" in
    let sprintf = Printf.sprintf in
    match k.x with
    | CNot Hole -> sprintf "not %s" hole
    | CBinOp1 (Hole, EMkTuple, expr, _env) ->
      let expr = Expr.to_string expr in
      sprintf "(%s, %s)" hole expr
    | CBinOp1 (Hole, op, expr, _env) ->
      let expr = Expr.to_string expr in
      sprintf "%s %s %s" hole (Ast.bin_op_to_string op) expr
    | CBinOp2 (value, EMkTuple, Hole) ->
      let value = Value.to_string value in
      sprintf "(%s, %s)" value hole
    | CBinOp2 (value, op, Hole) ->
      let value = Value.to_string value in
      sprintf "%s %s %s" value (Ast.bin_op_to_string op) hole
    | CIf (Hole, expr1, expr2, _env) ->
      let expr1 = Expr.to_string expr1 in
      let expr2 = Expr.to_string expr2 in
      sprintf "if %s then %s else %s" hole expr1 expr2
    | CLet (var, Hole, expr, _env) ->
      let var = Var.to_string var in
      let expr = Expr.to_string expr in
      sprintf "let %s := %s in\n%s" var hole expr
    | CLetRec (var, Hole, expr, _env) ->
      let var = Var.to_string var in
      let expr = Expr.to_string expr in
      sprintf "let %s := %s in\n%s" var hole expr
    | CApply1 (Hole, expr, _env) ->
      let expr = Expr.to_string expr in
      sprintf "%s@%s" hole expr
    | CApply2 (func, Hole) ->
      let func = Value.to_string func in
      sprintf "%s@%s" func hole
    | CPerform (eff, Hole, ks) ->
      let eff = Var.to_string eff in
      let captured_ks =
        ks
        |> List.rev
        |> List.map ~f:(stringify_kont ~source)
        |> String.concat ~sep:", "
        |> String.tr ~target:'\r' ~replacement:' '
        |> String.tr ~target:'\n' ~replacement:' '
      in
      let captured_ks =
        if Terminal.guess_printed_width captured_ks < 50
        then captured_ks
        else Int.to_string (List.length ks) ^ " konts"
      in
      sprintf "perform (%s %s) <%s>" eff hole captured_ks
    | CHandler (handlers, _env) ->
      let handled_effects =
        handlers
        |> List.map ~f:(fun handler -> Var.to_string handler.eff)
        |> String.concat ~sep:", "
      in
      sprintf "handler <%s>" handled_effects
    | CIOHandler -> "IOhandler"
  ;;

  let stringify_cek source { c; e; k; state_idx = _ } =
    let string_of_expr_or_val = function
      | CtrlExpr expr ->
        "T: "
        ^
        let _, s, _ = Ast.split_source_by_annotated source expr in
        s
      | CtrlValue value -> "V: " ^ Value.to_string value
    in
    let string_of_k = List.map ~f:(stringify_kont ~source) in
    Language.Ast.without_showing_anns (fun () ->
      [ string_of_expr_or_val c ], [ string_of_env e ], string_of_k k)
  ;;

  let print_cek ~source cek =
    Util.print_table
      ~header:("C", "E", "K")
      ~width_ratio:(4, 3, 5)
      ~stringify:(stringify_cek source)
      ~starting_idx:cek.state_idx
      [ cek ]
  ;;

  let rec debugger ?(print_state = false) ~source state =
    match state with
    | { history; current_cek; full_expr } ->
      if print_state then print_cek ~source current_cek;
      begin match prompt () with
      | Nop -> debugger ~source state
      | Help ->
        Stdio.print_string Debugger_cmd_parser.help_text;
        debugger ~source state
      | Continue -> driver ~ignore_bp:true ?times:None ~history current_cek
      | Stepover -> begin
        match current_cek.c with
        | CtrlExpr _ ->
          begin match current_cek.k with
          | [] -> ()
          | k :: _ -> k.breakpoint <- true
          end;
          driver ~ignore_bp:true ?times:None ~history current_cek
        | CtrlValue _ -> debugger ~print_state:true ~source state
      end
      | StepFwd n -> driver ~ignore_bp:true ~history ~times:n current_cek
      | StepBck n ->
        (* Pop the first n entries *)
        let new_history = List.drop history n in
        let new_cek =
          (* Want the (n-1)th state *)
          match List.nth history (n - 1) with
          | Some cek -> cek
          | None ->
            Stdio.print_endline "Tried to go back too far. Going back to start.";
            List.last_exn (current_cek :: history)
        in
        debugger
          ~print_state:true
          ~source
          { history = new_history; current_cek = new_cek; full_expr }
      | ShowState -> debugger ~print_state:true ~source state
      | Where ->
        let display_source annotated =
          let prefix, code_to_highlight, suffix =
            Ast.split_source_by_annotated source annotated
          in
          if !Sys.interactive
          then begin
            let highlighted =
              Util.with_ansi
                [ Terminal.Style.underline
                ; Terminal.Style.fg (Terminal.Color.ansi `green)
                ]
                code_to_highlight
            in
            String.concat [ prefix; highlighted; suffix ] |> Stdio.print_endline
          end
          else Stdio.print_endline code_to_highlight
        in
        let () =
          match current_cek.c with
          | CtrlExpr expr -> display_source expr
          | CtrlValue _ -> display_source (List.hd_exn current_cek.k)
        in
        debugger ~source state
      | BreakpointLoc (set, pos) ->
        let success = Ast.mark_breakpoint_loc ~set pos full_expr in
        if success
        then Stdio.print_endline "Breakpoint set."
        else Stdio.print_endline "Could not set breakpoint.";
        debugger ~source state
      | BreakpointEff (set, eff) ->
        let count = Ast.mark_perform ~set eff full_expr in
        if count > 0
        then Stdio.print_endline "Breakpoint set."
        else Stdio.print_endline "Could not set breakpoint.";
        debugger ~source state
      | BreakpointFun (set, fun_name) ->
        let count = Ast.mark_fun_app ~set fun_name full_expr in
        if count > 0
        then Stdio.print_endline "Breakpoint set."
        else Stdio.print_endline "Could not set breakpoint.";
        debugger ~source state
      | Inspect svar -> begin
        let var = Var.make svar in
        match Env.get var current_cek.e with
        | None ->
          Stdio.printf "Variable `%s` not present\n" svar;
          debugger ~source state
        | Some value ->
          begin match value with
          | VContinuation konts ->
            let stringified =
              konts
              |> List.map ~f:(stringify_kont ~source)
              |> String.concat ~sep:"\n--------------------------\n"
            in
            Stdio.print_endline stringified
          | _ ->
            let stringified =
              value |> Value.to_string |> Printf.sprintf "`%s`: %s" svar
            in
            Stdio.print_endline stringified
          end;
          debugger ~source state
      end
      end
  ;;

  let eval ?(break_at_start = true) ~source expr =
    let rec loop thunk =
      try thunk () with
      | Eval.Breakpoint (history, current_cek) ->
        loop
        @@ fun () ->
        debugger
          ~source
          ~print_state:true
          { history; current_cek; full_expr = expr }
    in
    let cek = Eval.initial_state expr in
    let value, history =
      loop (fun () ->
        driver ~history:[] ?times:(if break_at_start then Some 0 else None) cek)
    in
    value, Option.value_exn history
  ;;
end
