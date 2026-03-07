type cmd =
  | Help
  | BreakpointLoc of (int * int)
  | BreakpointEff of string
  | Continue
  | Where
  | Inspect of string
  | Nop
  | ShowState
  | Step

val help_text : string
val parse : string -> cmd
