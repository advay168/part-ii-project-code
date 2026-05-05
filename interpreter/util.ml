open! Base

type 'a triple = 'a * 'a * 'a

(** An ANSI code. *)
type ansi = Terminal.Style.t

(** Markup string with ANSI code for printing. *)
let with_ansi ansis s =
  let convert : ansi -> string = Terminal.Style.code in
  String.concat (List.map ~f:convert ansis) ^ s ^ convert Terminal.Style.none
;;

let repeat s n = String.concat (List.init n ~f:(Fn.const s))

(** Guesses width of string when printed in a terminal. *)
let utf_width = Terminal.guess_printed_width

(** Pads string to have a minimum width. *)
let utf_pad_right ~pad width s =
  let w = utf_width s in
  s ^ repeat pad (max 0 (width - w))
;;

(** Helper function to zip 4 lists padding shorter lists with empty strings. *)
let[@tail_mod_cons] rec zip4_longest lst =
  let uncons = function
    | [] -> "", []
    | x :: xs -> x, xs
  in
  match lst with
  | [], [], [], [] -> []
  | xs, ys, zs, ws ->
    let x, xs = uncons xs in
    let y, ys = uncons ys in
    let z, zs = uncons zs in
    let w, ws = uncons ws in
    let hd = x, y, z, w in
    let tl = xs, ys, zs, ws in
    hd :: zip4_longest tl
;;

(** Prints a table of three columns (plus an ascending index). *)
let print_table
      ~header:(h1, h2, h3)
      ~width_ratio:(r1, r2, r3)
      ~stringify
      ~starting_idx
      lst
  =
  let wn =
    (* Size of index column. *)
    let highest_n = starting_idx + List.length lst in
    String.length (Int.to_string highest_n)
  in
  let w1, w2, w3 =
    (* Size of each column according to specified ratio. *)
    let w =
      (Terminal.Size.get_columns () |> Option.value ~default:85) - 13 - wn
    in
    let ratio_total = r1 + r2 + r3 in
    w * r1 / ratio_total, w * r2 / ratio_total, w * r3 / ratio_total
  in
  let rows =
    (* Terminal rows to be printed. *)
    let break w xs =
      (* Wraps a string into a list of string so each has width [w] breaking on
         spaces. *)
      List.fold
        (String.split ~on:' ' xs |> List.rev)
        ~init:[ "" ]
        ~f:(fun lst x ->
          match lst with
          | [] -> assert false
          | current :: rest ->
            if utf_width current + utf_width x >= w
            then x :: current :: rest
            else (x ^ " " ^ current) :: rest)
    in
    let mapper w s =
      (* Run on each column. Intersperses logical row seperator string, splits
         on newlines and wraps each logical row into screen row. *)
      s
      |> List.intersperse ~sep:(repeat "─" (w - 1))
      |> List.concat_map ~f:String.split_lines
      |> List.concat_map ~f:(break w)
    in
    List.mapi
      ~f:(fun idx x ->
        let s1, s2, s3 = stringify x in
        ( [ Int.to_string @@ (starting_idx + idx) ]
        , mapper w1 s1
        , mapper w2 s2
        , mapper w3 s3 ))
      lst
  in
  let print_seperators left_sep mid_sep right_sep =
    let sep_string =
      String.concat
        [ left_sep
        ; String.concat ~sep:mid_sep
          @@ List.map ~f:(fun l -> repeat "─" (l + 2)) [ wn; w1; w2; w3 ]
        ; right_sep
        ]
    in
    Stdio.print_endline sep_string
  in
  let print_row horizontal_sep row =
    let go =
      fun (idx, a, b, c) ->
      Stdlib.Printf.printf
        "│ %s │ %s │ %s │ %s │\n"
        (utf_pad_right ~pad:" " wn idx)
        (utf_pad_right ~pad:" " w1 a)
        (utf_pad_right ~pad:" " w2 b)
        (utf_pad_right ~pad:" " w3 c)
    in
    if horizontal_sep then print_seperators "├" "┼" "┤";
    let screen_rows = zip4_longest row in
    List.iter ~f:go screen_rows
  in
  print_seperators "┌" "┬" "┐";
  print_row false ([ "n" ], [ h1 ], [ h2 ], [ h3 ]);
  rows |> List.iter ~f:(print_row true);
  print_seperators "└" "┴" "┘"
;;
