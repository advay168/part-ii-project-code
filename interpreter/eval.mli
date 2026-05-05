module Value : sig
  (** Type of Effektra values. *)
  type t

  val to_string : t -> string
end

module Eval : sig
  (** Raised when Effektra code tries to use an incorrect type in an operation.
 *)
  exception TypeError of string * Value.t

  (** Raised when an attempt is made to access an unbound Effektra variable. *)
  exception UnboundVarError of Language.Var.t * Value.t Env.t

  (** Raised when a [perform]ed effect is not handled. *)
  exception UnhandledEffect of Language.Var.t * Value.t

  (** Evaluate an Effektra AST (which was formed from [source]) to a value or
      loop indefinitely. *)
  val eval : source:string -> Language.Ast.expr -> Value.t

  (** Internal representation of a program state. *)
  type cek
end

module Debugger : sig
  (** Evaluate an Effektra AST (which was formed from [source]) to a value or
      loop indefinitely. Also returns a list of all intermediate [cek] states.
      [break_at_start] controls if the debugger is activated at the start; the
      default is true. *)
  val eval
    :  ?break_at_start:bool
    -> source:string
    -> Language.Ast.expr
    -> Value.t * Eval.cek list
end
