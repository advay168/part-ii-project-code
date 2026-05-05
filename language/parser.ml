open! Base

exception ParseError of string

(** Verifies that any compound effect handler does not handle the same effect
    name twice. *)
let rec validate_distinct_handlers (expr : Ast.expr) =
  match expr.x with
  | Ast.MkInt _ -> ()
  | Ast.MkBool _ -> ()
  | Ast.MkUnit -> ()
  | Ast.MkBinOp (e1, _, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | Ast.MkNot e -> validate_distinct_handlers e
  | Ast.MkIf (e1, e2, e3) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2;
    validate_distinct_handlers e3
  | Ast.MkVar _ -> ()
  | Ast.MkLet (_, e1, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | Ast.MkFun (_, e) -> validate_distinct_handlers e
  | Ast.MkApply (e1, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | Ast.MkPerform (_, e) -> validate_distinct_handlers e
  | Ast.MkHandle (e, hs) ->
    validate_distinct_handlers e;
    List.iter hs ~f:(fun h -> validate_distinct_handlers h.body);
    let names = List.map hs ~f:(fun h -> h.eff) in
    if List.contains_dup names ~compare:Var.compare
    then raise (ParseError "Repeated handler clauses")
;;

let parse ~filename string =
  let lexbuf = Lexing.from_string string in
  Lexing.set_filename lexbuf filename;
  match Parser_generator.prog Lexer.read lexbuf with
  | Some expr ->
    validate_distinct_handlers expr;
    expr
  | None -> raise (ParseError "Empty string")
  | exception Parser_generator.Error -> raise (ParseError "Parsing Error")
  | exception Lexer.SyntaxError err ->
    raise (ParseError ("Lexing error: " ^ err))
;;
