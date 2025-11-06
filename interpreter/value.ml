type t =
  | VInt of int
  | VBool of bool

let string_of_t = function
  | VInt int -> Int.to_string int
  | VBool bool -> Bool.to_string bool
;;
