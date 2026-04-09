module Value : sig
  type t

  val to_string : t -> string
end

module Eval : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of Language.Var.t * Value.t Env.t
  exception UnhandledEffect of Language.Var.t * Value.t

  val eval : source:string -> Language.Ast.expr -> Value.t

  type cek
end

module Debugger : sig
  val eval
    :  ?break_at_start:bool
    -> source:string
    -> Language.Ast.expr
    -> Value.t * Eval.cek list
end
