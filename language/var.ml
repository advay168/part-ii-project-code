open Base

type t = string [@@deriving sexp_of]

let equal = String.equal
