(** Polymorphic environment type (covariant). *)
type +'value t

val empty : 'value t
val get : Language.Var.t -> 'value t -> 'value option

(** Binds variable. Set [hidden] for builtin variables which should not be
    displayed. *)
val set : ?hidden:bool -> Language.Var.t -> 'value -> 'value t -> 'value t

(** Converts to string ignoring [hidden] variables. *)
val string_of_t : ('value -> string) -> 'value t -> string
