open! Base

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

let help_text =
  {|
Help for debugger commands:
- b/bp/break/breakpoint loc <line>:<col> -> Set breakpoint at location.
- b/bp/break/breakpoint eff <name>       -> Set breakpoint when effect <name> is performed.
- b/bp/break/breakpoint fun <name>       -> Set breakpoint when function <name> is going to be applied.
- r/run/c/continue                       -> Continue without debugging.
- w/where                                -> Highlight the source location of the current term.
- show/state/cek                         -> Print CEK state.
- i/inspect <var>                        -> Print the value of variable <var>.
- s/step <num>? (default 1)              -> Step the evaluation forwards <num> times.
- r/rev <num>? (default 1)               -> Step the evaluation in reverse <num> times.
- h/help                                 -> Print this help text.
|}
;;

let parse_help s =
  let re = Str.regexp {|^\(h\|help\|?\)$|} in
  Option.some_if (Str.string_match re s 0) Help
;;

let parse_breakpoint_loc s =
  let re =
    Str.regexp {|^\(b\|bp\|break\|breakpoint\) \([0-9]*\):\([0-9]*\)$|}
  in
  if Str.string_match re s 0
  then
    Some
      (BreakpointLoc
         ( Int.of_string (Str.matched_group 2 s)
         , Int.of_string (Str.matched_group 3 s) ))
  else None
;;

let ident_regex = "[a-zA-z_][0-9a-zA-z_]*"

let parse_breakpoint_eff s =
  let re =
    Str.regexp ({|^\(b\|bp\|break\|breakpoint\) eff \(|} ^ ident_regex ^ {|\)$|})
  in
  if Str.string_match re s 0
  then Some (BreakpointEff (Str.matched_group 2 s))
  else None
;;

let parse_breakpoint_fun s =
  let re =
    Str.regexp ({|^\(b\|bp\|break\|breakpoint\) fun \(|} ^ ident_regex ^ {|\)$|})
  in
  if Str.string_match re s 0
  then Some (BreakpointFun (Str.matched_group 2 s))
  else None
;;

let parse_continue s =
  let re = Str.regexp {|^\(r\|run\|c\|continue\)$|} in
  Option.some_if (Str.string_match re s 0) Continue
;;

let parse_where s =
  let re = Str.regexp {|^\(w\|where\)$|} in
  Option.some_if (Str.string_match re s 0) Where
;;

let parse_inspect s =
  let re = Str.regexp ({|^\(i\|inspect\) \($|} ^ ident_regex ^ {|\)$|}) in
  if Str.string_match re s 0
  then Some (Inspect (Str.matched_group 2 s))
  else None
;;

let parse_nop s =
  let re = Str.regexp {|^ *$|} in
  Option.some_if (Str.string_match re s 0) Nop
;;

let parse_show_state s =
  let re = Str.regexp {|^\(show\|state\|cek\)$|} in
  Option.some_if (Str.string_match re s 0) ShowState
;;

let parse_step_fwd s =
  let re = Str.regexp {|^\(s\|step\) ?\([0-9]+\)?$|} in
  if Str.string_match re s 0
  then (
    let num_steps =
      Option.try_with (fun () -> Int.of_string (Str.matched_group 2 s))
    in
    Some (StepFwd (Option.value ~default:1 num_steps)))
  else None
;;

let parse_step_bck s =
  let re = Str.regexp {|^\(r\|rev\) ?\([0-9]+\)?$|} in
  if Str.string_match re s 0
  then (
    let num_steps =
      Option.try_with (fun () -> Int.of_string (Str.matched_group 2 s))
    in
    Some (StepBck (Option.value ~default:1 num_steps)))
  else None
;;

let parse s =
  [ parse_help
  ; parse_breakpoint_loc
  ; parse_breakpoint_fun
  ; parse_breakpoint_eff
  ; parse_continue
  ; parse_where
  ; parse_inspect
  ; parse_nop
  ; parse_show_state
  ; parse_step_fwd
  ; parse_step_bck
  ]
  |> List.map ~f:(fun f -> f s)
  |> List.find ~f:Option.is_some
  |> Option.join
  |> Option.value_or_thunk ~default:(fun () ->
    Stdio.prerr_endline "Error: Could not parse debugger command";
    Nop)
;;
