type 'value t

val empty : 'value t
val get : Language.Var.t -> 'value t -> 'value option
val set : Language.Var.t -> 'value -> 'value t -> 'value t
