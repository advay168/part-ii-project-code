type cmd =
  | Help
  | BreakpointLoc of bool * (int * int)
  | BreakpointEff of bool * string
  | BreakpointFun of bool * string
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
