exception TypeError of string * Value.t
exception UnboundVarError of string * Value.t Env.t
exception LangException of Value.t

val cek_eval : Language.Ast.expr -> Value.t
val eval : Language.Ast.expr -> Value.t
