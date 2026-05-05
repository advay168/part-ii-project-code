open! Base

(** Controls whether the source location is present when converting to sexp. *)
let show_anns = ref true

(** Utility function that evaluates its argument thunk with [show_anns] set to
    false. *)
let without_showing_anns f = Ref.set_temporarily show_anns false ~f

(** Span of source code. *)
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

(** Makes an annotated value setting [breakpoint] to [false]. *)
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

(** Splits source code into [prefix, delimited, suffix] based on [span] field.
 *)
let split_source_by_annotated source expr =
  let start, end_ = expr.span in
  let prefix = String.sub source ~pos:0 ~len:start.pos_cnum in
  let delimited =
    String.sub source ~pos:start.pos_cnum ~len:(end_.pos_cnum - start.pos_cnum)
  in
  let suffix =
    String.sub
      source
      ~pos:end_.pos_cnum
      ~len:(String.length source - end_.pos_cnum)
  in
  prefix, delimited, suffix
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

(** Effektra AST *)
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
  { eff : Var.t (** Name of effect which is handled. *)
  ; arg : Var.t (** Name to which payload is to be bound. *)
  ; kont : Var.t (** Name to which the captured kontinuation is to be bound. *)
  ; body : expr (** Handler body. *)
  }
[@@deriving sexp_of]

(** Higher-order [marker] abstraction to set breakpoints under different
    conditions. *)
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

(** Sets a breakpoint in any AST node which performs the specified effect name.
 *)
let mark_perform ~set name =
  let var_name = Var.make name in
  marker ~set
  @@ function
  | MkPerform (eff, _) -> Var.equal var_name eff
  | _ -> false
;;

(** Sets a breakpoint in any AST node which applies a function of the given
    name. *)
let mark_fun_app ~set name =
  let var_name = Var.make name in
  marker ~set
  @@ function
  | MkApply ({ x = MkVar func; _ }, _) -> Var.equal var_name func
  | _ -> false
;;

(** Checks if source code position ([line, col]) is contained within a [span].
    The logic checks if [line] is contained and also checks if [col] is
    contained if [line] is on an endpoint of [span]. *)
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

(** Sets a breakpoint in atmost one AST node, if it is the most specific node
    spanning the argument position. This and the following functions take a
    [set] parameter which can be used to unset a breakpoint. *)
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

(** Utility functor module which takes a source code string and generates a
    module which uses the [span] field to display an [annotated] value to a
    string. Needed for using some ppx (preprocessing) extensions. *)
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
