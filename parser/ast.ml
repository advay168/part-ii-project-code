open Base

(* [annotated] is used to provide a custom sexpifier which is less verbose than
   that which would be generated. *)

type 't annotated = { loc : Lexing.position * Lexing.position; e : 't }

let sexp_of_annotated sexp_of_t { loc = startpos, endpos; e } =
  let loc_sexp =
    let sl, sc =
      (startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol + 1)
    in
    let el, ec = (endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol + 1) in
    let ( ! ) = Int.to_string in
    Sexp.Atom
      [%string {|<%{startpos.pos_fname}:{%{!sl}:%{!sc}..%{!el}:%{!ec}}>|}]
  in
  match sexp_of_t e with
  | Sexp.List lst -> Sexp.List (lst @ [ loc_sexp ])
  | Atom _ -> assert false
;;

type expr = expr' annotated

and expr' = MkInt of int | MkAdd of expr * expr | MkMult of expr * expr
[@@deriving sexp_of]
