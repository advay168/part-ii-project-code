(** Span of source code. *)
type span = Lexing.position * Lexing.position

val linecol_of_span : span -> (int * int) * (int * int)

type 't annotated =
  { span : span
  ; x : 't
  ; mutable breakpoint : bool
  }
[@@deriving sexp_of]

(** Makes an annotated value setting [breakpoint] to [false]. *)
val make : Lexing.position * Lexing.position -> 'a -> 'a annotated

(** Splits source code into [prefix, delimited, suffix] based on [span] field.
 *)
val split_source_by_annotated
  :  string
  -> 'a annotated
  -> string * string * string

(** Controls whether the source location is present when converting to sexp. *)
val show_anns : bool ref

(** Utility function that evaluates its argument thunk with [show_anns] set to
    false. *)
val without_showing_anns : (unit -> 'a) -> 'a

type binOp =
  | IAdd
  | IMul
  | IEql
  | BAnd
  | BOr
  | EMkTuple
[@@deriving sexp_of]

val bin_op_to_string : binOp -> string

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

(** Sets a breakpoint in any AST node which performs the specified effect name.
 *)
val mark_perform : set:bool -> string -> expr -> int

(** Sets a breakpoint in any AST node which applies a function of the given
    name. *)
val mark_fun_app : set:bool -> string -> expr -> int

(** Sets a breakpoint in atmost one AST node, if it is the most specific node
    spanning the argument position. This and the following functions take a
    [set] parameter which can be used to unset a breakpoint. *)
val mark_breakpoint_loc : set:bool -> int * int -> expr -> bool

(** Utility functor module which takes a source code string and generates a
    module which uses the [span] field to display an [annotated] value to a
    string. Needed for using some ppx (preprocessing) extensions. *)
module Make_to_string : functor
    (_ : sig
       val source : string
     end)
    -> sig
  val to_string : _ annotated -> string
end
