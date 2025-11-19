exception TypeError of string * Value.t
exception UnboundVarError of string * Value.t Store.t
exception LangException of Value.t

val eval : Value.t Store.t * Language.Ast.expr -> Value.t
