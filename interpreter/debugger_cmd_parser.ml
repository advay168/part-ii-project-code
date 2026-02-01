open! Base

type cmd =
  | Breakpoint of (int * int)
  | Continue
  | Where
  | Inspect of string
  | Nop
  | ShowState
  | Step
  | Help

let help_text =
  {|
Help for debugger commands:
- b/bp/break/breakpoint line:col -> Set breakpoint at location.
- r/run/c/continue               -> Continue without debugging.
- w/where                        -> Highlight the source location of the current expression.
- show/state/cek                 -> Print CEK state.
- i/inspect var                  -> Print the value of variable `var`.
- s/step                         -> Step the evaluation.
- h/help                         -> Print this help text.
|}
;;

let parse_step s =
  let re = Str.regexp {|^\(s\|step\)$|} in
  Option.some_if (Str.string_match re s 0) Step
;;

let parse_help s =
  let re = Str.regexp {|^\(h\|help\|?\)$|} in
  Option.some_if (Str.string_match re s 0) Help
;;

let parse_bp s =
  let re =
    Str.regexp {|^\(b\|bp\|break\|breakpoint\) \([0-9]*\):\([0-9]*\)$|}
  in
  if Str.string_match re s 0
  then
    Some
      (Breakpoint
         ( Int.of_string (Str.matched_group 2 s)
         , Int.of_string (Str.matched_group 3 s) ))
  else None
;;

let parse_inspect s =
  let re = Str.regexp {|^\(i\|inspect\) \(.*\)$|} in
  if Str.string_match re s 0
  then Some (Inspect (Str.matched_group 2 s))
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

let parse_nop s =
  let re = Str.regexp {|^ *$|} in
  Option.some_if (Str.string_match re s 0) Nop
;;

let parse_showstate s =
  let re = Str.regexp {|^\(show\|state\|cek\)$|} in
  Option.some_if (Str.string_match re s 0) ShowState
;;

let parse s =
  [ parse_bp
  ; parse_inspect
  ; parse_continue
  ; parse_where
  ; parse_nop
  ; parse_showstate
  ; parse_help
  ; parse_step
  ]
  |> List.map ~f:(fun f -> f s)
  |> List.find ~f:Option.is_some
  |> Option.join
  |> Option.value_or_thunk ~default:(fun () ->
    Stdio.prerr_endline "Error: Could not parse debugger command";
    Nop)
;;
