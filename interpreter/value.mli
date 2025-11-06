type t =
  | VInt of int
  | VBool of bool

val string_of_t : t -> string
