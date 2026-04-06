open Base

(** Abstract variable type. *)
type t

val make : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val to_ignore : t -> bool
val sexp_of_t : t -> Sexp.t
