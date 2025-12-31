type +'value t

val string_of_t : ('value -> string) -> 'value t -> string
val empty : 'value t
val get : Language.Var.t -> 'value t -> 'value option
val set : Language.Var.t -> 'value -> 'value t -> 'value t
