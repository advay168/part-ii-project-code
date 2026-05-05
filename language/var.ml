open Base

type t = string

let make s = s
let to_string v = v
let equal = String.equal
let compare = String.compare
let sexp_of_t t = Sexp.Atom ("'" ^ t ^ "'")
let to_ignore v = String.is_prefix ~prefix:"_" v
