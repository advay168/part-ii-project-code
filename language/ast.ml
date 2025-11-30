open Base

(* [annotated] is used to provide a custom sexpifier which is less verbose than
   that which would be generated. *)
type 't annotated =
  { loc : Lexing.position * Lexing.position
  ; e : 't
  }

let show_locs = ref true

let without_showing_locs f =
  let s = !show_locs in
  show_locs := false;
  let x = f () in
  show_locs := s;
  x
;;

let sexp_of_annotated sexp_of_t { loc = startpos, endpos; e } =
  let loc_sexp =
    let sl, sc = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol + 1 in
    let el, ec = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol + 1 in
    let ( ! ) = Int.to_string in
    Sexp.Atom
      [%string {|<%{startpos.pos_fname}:{%{!sl}:%{!sc}..%{!el}:%{!ec}}>|}]
  in
  match sexp_of_t e with
  | Sexp.List lst -> Sexp.List (if !show_locs then lst @ [ loc_sexp ] else lst)
  | Atom _ -> assert false
;;

type binOp =
  | IAdd
  | IMul
  | IEql
  | BAnd
  | BOr
[@@deriving sexp_of]

type expr = expr' annotated

and expr' =
  | MkInt of int
  | MkBinOp of expr * binOp * expr
  | MkBool of bool
  | MkNot of expr
  | MkIf of expr * expr * expr
  | MkVar of Var.t
  | MkLet of Var.t * expr * expr
  | MkFun of Var.t * expr
  | MkApply of expr * expr
  | MkPerform of expr
  | MkHandle of expr * Var.t * Var.t * expr
[@@deriving sexp_of]
