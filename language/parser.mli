exception ParseError of string

(** Parses Effektra code from the second parameter setting [filename] in the
    metadata. *)
val parse : filename:string -> string -> Ast.expr
