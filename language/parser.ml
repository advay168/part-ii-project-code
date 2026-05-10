open! Base

exception ParseError of string

(** Verifies that any compound effect handler does not handle the same effect
    name twice. *)
let rec validate_distinct_handlers (expr : Ast.expr) =
  match expr.x with
  | MkInt _ -> ()
  | MkBool _ -> ()
  | MkUnit -> ()
  | MkBinOp (e1, _, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | MkUnary (_, e) -> validate_distinct_handlers e
  | MkIf (e1, e2, e3) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2;
    validate_distinct_handlers e3
  | MkVar _ -> ()
  | MkLet (_, e1, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | MkFun (_, e) -> validate_distinct_handlers e
  | MkApply (e1, e2) ->
    validate_distinct_handlers e1;
    validate_distinct_handlers e2
  | MkPerform (_, e) -> validate_distinct_handlers e
  | MkHandle (e, hs) ->
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
