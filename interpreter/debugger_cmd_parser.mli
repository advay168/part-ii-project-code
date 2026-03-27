type cmd =
  | Help
  | BreakpointLoc of (int * int)
  | BreakpointEff of string
  | BreakpointFun of string
  | Continue
  | Where
  | Inspect of string
  | Nop
  | ShowState
  | StepFwd of int
  | StepBck of int
  | Stepover

val help_text : string
val parse : string -> cmd
