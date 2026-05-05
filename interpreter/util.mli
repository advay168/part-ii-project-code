(** An ANSI code. *)
type ansi = Terminal.Style.t

(** Markup string with ANSI code for printing. *)
val with_ansi : ansi list -> string -> string

type 'a triple = 'a * 'a * 'a

(** Prints a table of three columns (plus an ascending index). *)
val print_table
  :  header:string triple
  -> width_ratio:int triple
  -> stringify:('a -> string list triple)
  -> starting_idx:int
  -> 'a list
  -> unit
