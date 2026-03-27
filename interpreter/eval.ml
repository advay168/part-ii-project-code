open! Base
open Language

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
  val to_string : t -> string
end = struct
  type t =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTup of t * t
    | VFun of Var.t * Ast.expr * t Env.t lazy_t
    | VFunBuiltin of Builtins.t
    | VContinuation of Main.kontinuation list

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
end

and Main : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  type kontinuation = kontinuation' Ast.annotated
  and kontinuation'

  val eval : debug:bool -> source:string -> Language.Ast.expr -> Value.t
end = struct
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception UnhandledEffect of Var.t * Value.t

  type expr_or_val =
    | Expr of Ast.expr
    | Value of Value.t
  [@@deriving sexp_of]

  (** Solely for readability to denote where the continuation expects a hole.  *)
  type hole = Hole [@@deriving sexp_of]

  (** Override [Var] printing to make it easier to read in debugging. *)
  module Var = struct
    include Var

    let sexp_of_t t = Sexp.Atom ("'" ^ t ^ "'")
  end

  type env = Value.t Env.t

  let string_of_env = Env.string_of_t Value.to_string
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
             | Some k -> { e with x = k; breakpoint = false } :: cs)
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

  let rec translate_kont ~source (k : kontinuation) =
    let op_to_string = function
      | Ast.IAdd -> "+"
      | IMul -> "*"
      | IEql -> "="
      | BAnd -> "&&"
      | BOr -> "||"
      | EMkTuple -> ","
    in
    let module Expr =
      Ast.Make_to_string (struct
        let source = source
      end)
    in
    let hole = "⬤" in
    (* TODO: Show [env]. *)
    match k.x with
    | CNot Hole -> [%string "not *"]
    | CBinOp1 (Hole, op, expr, _env) ->
      [%string "%{hole} %{op_to_string op} %{expr#Expr}"]
    | CBinOp2 (value, op, Hole) ->
      [%string "%{value#Value} %{op_to_string op} %{hole}"]
    | CIf (Hole, expr1, expr2, _env) ->
      [%string "if %{hole} then %{expr1#Expr} else %{expr2#Expr}"]
    | CLet (var, Hole, expr, _env) ->
      [%string "let %{var} := %{hole} in\n%{expr#Expr}"]
    | CLetRec (var, Hole, expr, _env) ->
      [%string "let %{var} := %{hole} in\n%{expr#Expr}"]
    | CApply1 (Hole, expr, _env) -> [%string "%{hole}@%{expr#Expr}"]
    | CApply2 (func, Hole) -> [%string "%{func#Value}@%{hole}"]
    | CPerform (eff, Hole, ks) ->
      let captured_ks =
        ks
        |> List.rev
        |> List.map ~f:(translate_kont ~source)
        |> String.concat ~sep:", "
        |> String.tr ~target:'\r' ~replacement:' '
        |> String.tr ~target:'\n' ~replacement:' '
      in
      let captured_ks =
        if Wcwidth.wcswidth captured_ks < 50
        then captured_ks
        else Int.to_string (List.length ks) ^ " konts"
      in
      [%string "perform (%{eff} %{hole}) <%{captured_ks}>"]
    | CHandler (handlers, _env) ->
      let handled_effects =
        handlers
        |> List.map ~f:(fun handler -> handler.eff)
        |> String.concat ~sep:", "
      in
      [%string "handler <%{handled_effects}>"]
  ;;

  let stringify source { c; e; k } =
    let string_of_expr_or_val = function
      | Expr expr ->
        "T: "
        ^
        let _, s, _ = Ast.split_source_by_annotated source expr in
        s
      | Value value -> "V: " ^ Value.to_string value
    in
    let string_of_k = List.map ~f:(translate_kont ~source) in
    Language.Ast.without_showing_anns (fun () ->
      [ string_of_expr_or_val c ], [ string_of_env e ], string_of_k k)
  ;;

  type history = t list

  exception Breakpoint of history * t

  (** [times] refers to the number of times to run before breakpointing. 
      [ignore_bp] means to not trigger any AST breakpoints on this step. Useful to continue execution from the debugger.
   *)
  let rec driver ?(ignore_bp = false) ~history:old_history ~times state =
    let times =
      match times with
      | Some 0 when not ignore_bp -> raise (Breakpoint (old_history, state))
      | Some n -> Some (n - 1)
      | None -> None
    in
    let history = state :: old_history in
    match state with
    | { c = Value v; e = _; k = [] } -> v, history
    | { c = Value _; e = _; k = [ { breakpoint = true; _ } ] }
    | { c = Expr { breakpoint = true; _ }; _ }
      when not ignore_bp ->
      Stdio.print_endline "Breakpoint hit";
      raise (Breakpoint (old_history, state))
    | _ -> driver ~history ~times (step state)
  ;;

  module Debugger = struct
    type dbg_state =
      { history : history
      ; current_cek : t
      ; full_expr : Ast.expr
      }

    let prompt () =
      Stdio.print_string "debugger> ";
      Debugger_cmd_parser.parse (Stdio.In_channel.input_line_exn Stdio.stdin)
    ;;

    let rec debugger ?(print_state = false) ~source state =
      match state with
      | { history; current_cek; full_expr } ->
        if print_state
        then
          Util.print_table
            ~header:("C", "E", "K")
            ~stringify:(stringify source)
            [ current_cek ];
        begin match prompt () with
        | Nop -> debugger ~source state
        | Help ->
          Stdio.print_string Debugger_cmd_parser.help_text;
          debugger ~source state
        | Continue -> driver ~ignore_bp:true ~times:None ~history current_cek
        | Stepover ->
          begin match current_cek.k with
          | [] -> ()
          | k :: _ -> k.breakpoint <- true
          end;
          driver ~ignore_bp:true ~times:None ~history current_cek
        | StepFwd n ->
          driver ~ignore_bp:true ~history ~times:(Some n) current_cek
        | StepBck n ->
          (* Pop the first n entries *)
          let new_history = List.drop history n in
          let new_cek =
            (* Want the (n-1)th state *)
            match List.nth history (n - 1) with
            | Some cek -> cek
            | None ->
              Stdio.print_endline
                "Tried to go back too far. Going back to start.";
              List.last_exn (current_cek :: history)
          in
          debugger
            ~print_state:true
            ~source
            { history = new_history; current_cek = new_cek; full_expr }
        | ShowState -> debugger ~print_state:true ~source state
        | Where ->
          let () =
            match current_cek.c with
            | Value _ ->
              let prefix, here, suffix =
                Ast.split_source_by_annotated source (List.hd_exn current_cek.k)
              in
              String.concat
                [ prefix
                ; here |> Util.with_ansi [ Underline; GreenFG ]
                ; suffix
                ]
              |> Stdio.print_endline
            | Expr expr ->
              let prefix, here, suffix =
                Ast.split_source_by_annotated source expr
              in
              String.concat
                [ prefix
                ; here |> Util.with_ansi [ Underline; GreenFG ]
                ; suffix
                ]
              |> Stdio.print_endline
          in
          debugger ~source state
        | BreakpointLoc pos ->
          let success = Ast.mark_breakpoint pos full_expr in
          if success
          then Stdio.print_endline "Breakpoint set."
          else Stdio.print_endline "Could not set breakpoint.";
          debugger ~source state
        | BreakpointEff eff ->
          let count = Ast.mark_perform eff full_expr in
          if count > 0
          then Stdio.print_endline "Breakpoint set."
          else Stdio.print_endline "Could not set breakpoint.";
          debugger ~source state
        | BreakpointFun fun_name ->
          let count = Ast.mark_fun_app fun_name full_expr in
          if count > 0
          then Stdio.print_endline "Breakpoint set."
          else Stdio.print_endline "Could not set breakpoint.";
          debugger ~source state
        | Inspect var -> begin
          match Env.get var current_cek.e with
          | None ->
            Stdio.print_endline "Variable not present";
            debugger ~source state
          | Some value ->
            begin match value with
            | VContinuation konts ->
              konts
              |> List.map ~f:(translate_kont ~source)
              |> String.concat ~sep:"\n--------------------------\n"
              |> Stdio.print_endline
            | _ ->
              value
              |> Value.to_string
              |> Printf.sprintf "`%s`: %s" var
              |> Stdio.print_endline
            end;
            debugger ~source state
        end
        end
    ;;
  end

  let eval ~debug ~source expr =
    let cek = { c = Expr expr; e = builtins_env; k = [] } in
    let rec loop thunk =
      try thunk () with
      | Breakpoint (history, current_cek) ->
        loop
        @@ fun () ->
        Debugger.debugger
          ~source
          ~print_state:true
          { history; current_cek; full_expr = expr }
    in
    let evaluated_value, history =
      loop (fun () -> driver ~history:[] ~times:(Option.some_if debug 0) cek)
    in
    ignore history;
    evaluated_value
  ;;
end
