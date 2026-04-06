open Base

type t = string

let make s = s
let to_string v = v
let equal = String.equal
let to_ignore v = String.is_prefix ~prefix:"_" v
let sexp_of_t t = Sexp.Atom ("'" ^ t ^ "'")
