module Value : sig
  type t

  val string_of_t : t -> string
end

module Main : sig
  exception TypeError of string * Value.t
  exception UnboundVarError of string * Value.t Env.t
  exception UnhandledEffect of Language.Var.t * Value.t

  val eval : debug:bool -> source:string -> Language.Ast.expr -> Value.t
end
