(** Enum of debugger commands. *)
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

(** Text to be printed with help command. *)
val help_text : string

(** Parses string into debugger command. Returns [Nop] if cannot parse into
    valid command. *)
val parse : string -> cmd
