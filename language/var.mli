open Base

(** Abstract type representing Effektra variables. *)
type t

val make : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t

(** Check if starts with underscore. *)
val to_ignore : t -> bool
