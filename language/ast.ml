open! Base

let show_anns = ref true
let without_showing_anns f = Ref.set_temporarily show_anns false ~f

type span = Lexing.position * Lexing.position

let linecol_of_span ((startpos, endpos) : span) =
  let sl = startpos.pos_lnum in
  let sc = startpos.pos_cnum - startpos.pos_bol + 1 in
  let el = endpos.pos_lnum in
  let ec = endpos.pos_cnum - endpos.pos_bol + 1 in
  (sl, sc), (el, ec)
;;

type 't annotated =
  { span : span
  ; x : 't
  ; mutable breakpoint : bool
  }

let make span x = { span; x; breakpoint = false }

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

let split_source_by_annotated source expr =
  let start, end_ = expr.span in
  let prefix = String.sub source ~pos:0 ~len:start.pos_cnum in
  let middle =
    String.sub source ~pos:start.pos_cnum ~len:(end_.pos_cnum - start.pos_cnum)
  in
  let suffix =
    String.sub
      source
      ~pos:end_.pos_cnum
      ~len:(String.length source - end_.pos_cnum)
  in
  prefix, middle, suffix
;;

type binOp =
  | IAdd
  | IMul
  | IEql
  | BAnd
  | BOr
  | EMkTuple
[@@deriving sexp_of]

let bin_op_to_string = function
  | IAdd -> "+"
  | IMul -> "*"
  | IEql -> "="
  | BAnd -> "&&"
  | BOr -> "||"
  | EMkTuple -> ","
;;

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

let rec marker ~set (f : expr' -> bool) (e : expr) =
  let count =
    if f e.x
    then (
      e.breakpoint <- set;
      1)
    else 0
  in
  let rec_count =
    begin match e.x with
    | MkInt _ -> 0
    | MkBool _ -> 0
    | MkUnit -> 0
    | MkBinOp (e1, _, e2) -> marker ~set f e1 + marker ~set f e2
    | MkNot e -> marker ~set f e
    | MkIf (e1, e2, e3) ->
      marker ~set f e1 + marker ~set f e2 + marker ~set f e3
    | MkVar _ -> 0
    | MkLet (_, e1, e2) -> marker ~set f e1 + marker ~set f e2
    | MkFun (_, e) -> marker ~set f e
    | MkApply (e1, e2) -> marker ~set f e1 + marker ~set f e2
    | MkPerform (_, e) -> marker ~set f e
    | MkHandle (e, hs) ->
      marker ~set f e
      + List.sum (module Int) ~f:(fun h -> marker ~set f h.body) hs
    end
  in
  count + rec_count
;;

let mark_perform ~set name =
  let var_name = Var.make name in
  marker ~set
  @@ function
  | MkPerform (eff, _) -> Var.equal var_name eff
  | _ -> false
;;

let mark_fun_app ~set name =
  let var_name = Var.make name in
  marker ~set
  @@ function
  | MkApply ({ x = MkVar func; _ }, _) -> Var.equal var_name func
  | _ -> false
;;

let within (line, col) (span : span) =
  let (sl, sc), (el, ec) = linecol_of_span span in
  if line < sl || line > el
  then false
  else if line = sl && col < sc
  then false
  else if line = el && col >= ec
  then false
  else true
;;

let rec mark_breakpoint_loc ~set loc (e : expr) : bool =
  if not (within loc e.span)
  then false
  else begin
    let found_in_subterm =
      begin match e.x with
      | MkInt _ -> false
      | MkBool _ -> false
      | MkUnit -> false
      | MkBinOp (e1, _, e2) ->
        mark_breakpoint_loc ~set loc e1 || mark_breakpoint_loc ~set loc e2
      | MkNot e -> mark_breakpoint_loc ~set loc e
      | MkIf (e1, e2, e3) ->
        mark_breakpoint_loc ~set loc e1
        || mark_breakpoint_loc ~set loc e2
        || mark_breakpoint_loc ~set loc e3
      | MkVar _ -> false
      | MkLet (_, e1, e2) ->
        mark_breakpoint_loc ~set loc e1 || mark_breakpoint_loc ~set loc e2
      | MkFun (_, e) -> mark_breakpoint_loc ~set loc e
      | MkApply (e1, e2) ->
        mark_breakpoint_loc ~set loc e1 || mark_breakpoint_loc ~set loc e2
      | MkPerform (_, e) -> mark_breakpoint_loc ~set loc e
      | MkHandle (e, hs) ->
        mark_breakpoint_loc ~set loc e
        || List.exists hs ~f:(fun h -> mark_breakpoint_loc ~set loc h.body)
      end
    in
    if not found_in_subterm then e.breakpoint <- set else ();
    true
  end
;;

module Make_to_string =
functor
  (M : sig
     val source : string
   end)
  ->
  struct
    let to_string expr =
      let _, s, _ = split_source_by_annotated M.source expr in
      s
      |> String.split_lines
      |> List.map ~f:String.strip
      |> String.concat ~sep:"\n"
    ;;
  end
