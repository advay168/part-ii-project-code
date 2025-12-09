val print_table
  :  header:string * string * string
  -> stringify:('a -> string list * string list * string list)
  -> 'a list
  -> unit
