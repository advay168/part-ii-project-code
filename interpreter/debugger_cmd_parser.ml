open! Base

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

let help_text =
  {|Help for debugger commands:
- h/help                                 -> Print this help text.
- b/bp/break/breakpoint eff <name>       -> Set a breakpoint for when effect <name> is performed.
- u/unset eff <name>                     -> Unset breakpoint for effect <name>.
- b/bp/break/breakpoint fun <name>       -> Set breakpoint for when function <name> is applied.
- u/unset fun <name>                     -> Unset breakpoint for function <name>.
- b/bp/break/breakpoint loc <line>:<col> -> Set breakpoint for code at location.
- u/unset loc <line>:<col>               -> Unset breakpoint at location.
- c/continue                             -> Exit debugger and continue running.
- s/step <num>? (default 1)              -> Step the evaluation forwards <num> times.
- r/rev <num>? (default 1)               -> Step the evaluation in reverse <num> times.
- o/over                                 -> Step over evaluation of the current term.
- w/where                                -> Highlight the source location of the current term.
- show/state/cek                         -> Print CEK state.
- i/inspect <var>                        -> Print the value of variable <var>.
|}
;;

let run_regex s ~re ~group_nums =
  let re = Str.regexp ("^" ^ re ^ "$") in
  if Str.string_match re s 0
  then Some (List.map group_nums ~f:(fun n -> Str.matched_group n s))
  else None
;;

let parse_help s =
  match run_regex ~re:{|\(h\|help\|?\)|} ~group_nums:[] s with
  | Some [] -> Some Help
  | _ -> None
;;

let set_unset_bp_helper s ~set_re ~unset_re ~group_nums =
  let r1 = run_regex ~re:set_re ~group_nums s in
  let r2 = run_regex ~re:unset_re ~group_nums s in
  match r1, r2 with
  | Some x, _ -> Some (true, x)
  | _, Some x -> Some (false, x)
  | None, None -> None
;;

let parse_breakpoint_loc s =
  match
    set_unset_bp_helper
      ~set_re:{|\(b\|bp\|break\|breakpoint\) \([0-9]*\):\([0-9]*\)|}
      ~unset_re:{|\(u\|unset\) \([0-9]*\):\([0-9]*\)|}
      ~group_nums:[ 2; 3 ]
      s
  with
  | Some (set, [ row; col ]) ->
    let row, col = Int.of_string row, Int.of_string col in
    Some (BreakpointLoc (set, (row, col)))
  | _ -> None
;;

let ident_group = {|\([a-zA-z_][0-9a-zA-z_]*\)|}

let parse_breakpoint_eff s =
  match
    set_unset_bp_helper
      ~set_re:({|\(b\|bp\|break\|breakpoint\) eff |} ^ ident_group)
      ~unset_re:({|\(u\|unset\) eff |} ^ ident_group)
      ~group_nums:[ 2 ]
      s
  with
  | Some (set, [ eff ]) -> Some (BreakpointEff (set, eff))
  | _ -> None
;;

let parse_breakpoint_fun s =
  match
    set_unset_bp_helper
      ~set_re:({|\(b\|bp\|break\|breakpoint\) fun |} ^ ident_group)
      ~unset_re:({|\(u\|unset\) fun |} ^ ident_group)
      ~group_nums:[ 2 ]
      s
  with
  | Some (set, [ eff ]) -> Some (BreakpointFun (set, eff))
  | _ -> None
;;

let parse_continue s =
  match run_regex ~re:{|\(c\|continue\)|} ~group_nums:[] s with
  | Some [] -> Some Continue
  | _ -> None
;;

let parse_where s =
  match run_regex ~re:{|\(w\|where\)|} ~group_nums:[] s with
  | Some [] -> Some Where
  | _ -> None
;;

let parse_inspect s =
  match
    run_regex ~re:({|\(i\|inspect\) |} ^ ident_group) ~group_nums:[ 2 ] s
  with
  | Some [ var ] -> Some (Inspect var)
  | _ -> None
;;

let parse_nop s =
  match run_regex ~re:{| *|} ~group_nums:[] s with
  | Some [] -> Some Nop
  | _ -> None
;;

let parse_show_state s =
  match run_regex ~re:{|\(show\|state\|cek\)|} ~group_nums:[] s with
  | Some [] -> Some ShowState
  | _ -> None
;;

let parse_step_over s =
  match run_regex ~re:{|\(o\|over\)|} ~group_nums:[] s with
  | Some [] -> Some Stepover
  | _ -> None
;;

(* Doesn't use [run_regex] because of optional arg. *)
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

(* Doesn't use [run_regex] because of optional arg. *)
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

let parsers =
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
  ; parse_step_over
  ]
;;

let parse s =
  let parsed =
    parsers
    |> List.map ~f:(fun f -> f s)
    |> List.find ~f:Option.is_some
    |> Option.join
  in
  match parsed with
  | Some parsed -> parsed
  | None ->
    Stdio.prerr_endline "Error: Could not parse the debugger command";
    Nop
;;
