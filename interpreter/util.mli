type ansi =
  | Bold
  | Italic
  | Underline
  | Blink
  | RedFG
  | GreenFG
  | BlueFG
  | RedBG
  | GreenBG
  | BlueBG

val with_ansi : ansi list -> string -> string

val print_table
  :  header:string * string * string
  -> stringify:('a -> string list * string list * string list)
  -> 'a list
  -> unit
