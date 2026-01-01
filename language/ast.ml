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
    Sexp.Atom [%string {|<%{!sl}:%{!sc}..%{!el}:%{!ec}>|}]
  in
  ((match sexp_of_t e with
    | Sexp.List lst -> lst
    | Atom _ as s -> [ s ])
   @ if !show_locs then [ loc_sexp ] else [])
  |> Sexp.List
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
  | MkHandle of expr * handler annotated list

and handler =
  { eff : Var.t
  ; arg : Var.t
  ; kont : Var.t
  ; body : expr
  }
[@@deriving sexp_of]
