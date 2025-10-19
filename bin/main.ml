open! Base

let () =
  let filename = (Sys.get_argv ()).(1) in
  let code = Stdio.In_channel.read_all filename in
  let parsed = Parser.parse code in
  Stdio.print_endline parsed
;;
