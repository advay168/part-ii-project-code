type ansi = Terminal.Style.t

val with_ansi : ansi list -> string -> string

type 'a triple = 'a * 'a * 'a

val print_table
  :  header:string triple
  -> width_ratio:int triple
  -> stringify:('a -> string list triple)
  -> starting_idx:int
  -> 'a list
  -> unit
