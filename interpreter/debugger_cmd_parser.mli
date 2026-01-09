type cmd =
  | Breakpoint of (int * int)
  | Continue
  | Where
  | Inspect of string
  | Nop
  | ShowState
  | Step
  | Help

val help_text : string
val parse : string -> cmd
