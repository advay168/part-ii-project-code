type t = string [@@deriving sexp_of]

val equal : t -> t -> bool
