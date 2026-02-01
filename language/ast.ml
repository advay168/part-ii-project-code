open Base

type span = Lexing.position * Lexing.position

let linecol_of_span ((startpos, endpos) : span) =
  let sl, sc = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol + 1 in
  let el, ec = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol + 1 in
  (sl, sc), (el, ec)
;;

type 't annotated =
  { span : span
  ; x : 't
  ; mutable breakpoint : bool
  }

let make span x = { span; x; breakpoint = false }
let show_anns = ref true
let without_showing_anns f = Ref.set_temporarily show_anns false ~f

(* Custom sexpifier which is less verbose than that which would be generated. *)
let sexp_of_annotated sexp_of_t { span; x; breakpoint } =
  let ann_sexp =
    let (sl, sc), (el, ec) = linecol_of_span span in
    let ( ! ) = Int.to_string in
    Sexp.Atom
      ((if breakpoint then "<#" else "<")
       ^ [%string {|%{!sl}:%{!sc}..%{!el}:%{!ec}>|}])
  in
  ((match sexp_of_t x with
    | Sexp.List lst -> lst
    | Atom _ as s -> [ s ])
   @ if !show_anns then [ ann_sexp ] else [])
  |> Sexp.List
;;

let split_source_by_expr source expr =
  let start, end_ = expr.span in
  ( String.sub source ~pos:0 ~len:start.pos_cnum
  , String.sub source ~pos:start.pos_cnum ~len:(end_.pos_cnum - start.pos_cnum)
  , String.sub
      source
      ~pos:end_.pos_cnum
      ~len:(String.length source - end_.pos_cnum) )
;;

type binOp =
  | IAdd
  | IMul
  | IEql
  | BAnd
  | BOr
  | EMkTuple
[@@deriving sexp_of]

type expr = expr' annotated

and expr' =
  | MkInt of int
  | MkBool of bool
  | MkUnit
  | MkBinOp of expr * binOp * expr
  | MkNot of expr
  | MkIf of expr * expr * expr
  | MkVar of Var.t
  | MkLet of Var.t * expr * expr
  | MkFun of Var.t * expr
  | MkApply of expr * expr
  | MkPerform of Var.t * expr
  | MkHandle of expr * handler list

and handler =
  { eff : Var.t
  ; arg : Var.t
  ; kont : Var.t
  ; body : expr
  }
[@@deriving sexp_of]

let within (line, col) ((startpos, endpos) : span) =
  let sl, sc = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol + 1 in
  let el, ec = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol + 1 in
  sl <= line && line <= el && (sl <> el || (sc <= col && col < ec))
;;

let rec mark_breakpoint loc (e : expr) : bool =
  if not (within loc e.span)
  then false
  else begin
    if
      not
        begin match e.x with
        | MkInt _ -> false
        | MkBool _ -> false
        | MkUnit -> false
        | MkBinOp (e1, _, e2) ->
          mark_breakpoint loc e1 || mark_breakpoint loc e2
        | MkNot e -> mark_breakpoint loc e
        | MkIf (e1, e2, e3) ->
          mark_breakpoint loc e1
          || mark_breakpoint loc e2
          || mark_breakpoint loc e3
        | MkVar _ -> false
        | MkLet (_, e1, e2) -> mark_breakpoint loc e1 || mark_breakpoint loc e2
        | MkFun (_, e) -> mark_breakpoint loc e
        | MkApply (e1, e2) -> mark_breakpoint loc e1 || mark_breakpoint loc e2
        | MkPerform (_, e) -> mark_breakpoint loc e
        | MkHandle (e, hs) ->
          mark_breakpoint loc e
          || List.exists hs ~f:(fun h -> mark_breakpoint loc h.body)
        end
    then e.breakpoint <- true;
    true
  end
;;
