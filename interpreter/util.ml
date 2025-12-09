open! Base

let repeat s n = String.concat (List.init n ~f:(Fn.const s))

let print_table ~header:(ch, eh, kh) ~stringify lst =
  let w = 140 in
  let w1, w2, w3 = w * 4 / 12, w * 3 / 12, w * 5 / 12 in
  let break w xs =
    List.fold
      (String.split ~on:' ' xs |> List.rev)
      ~init:[ "" ]
      ~f:(fun lst x ->
        match lst with
        | [] -> assert false
        | current :: rest ->
          if String.length current + String.length x >= w
          then x :: current :: rest
          else (x ^ " " ^ current) :: rest)
  in
  let rows =
    List.map
      ~f:(fun x ->
        let s1, s2, s3 = stringify x in
        ( List.concat_map ~f:(break w1) s1
        , List.concat_map ~f:(break w2) s2
        , List.concat_map ~f:(break w3) s3 ))
      lst
  in
  let print_seps left mid right =
    Stdio.print_endline
      (left
       ^ String.concat
           ~sep:mid
           (List.map ~f:(fun l -> repeat "─" (l + 2)) [ w1; w2; w3 ])
       ^ right)
  in
  let print_row b row =
    let go =
      fun (c, e, k) ->
      Stdlib.Printf.printf "│ %-*s │ %-*s │ %-*s │\n" w1 c w2 e w3 k
    in
    let[@tail_mod_cons] rec zip3_longest lst =
      let uncons = function
        | [] -> "", []
        | x :: xs -> x, xs
      in
      match lst with
      | [], [], [] -> []
      | xs, ys, zs ->
        let x, xs = uncons xs in
        let y, ys = uncons ys in
        let z, zs = uncons zs in
        (x, y, z) :: zip3_longest (xs, ys, zs)
    in
    if b then print_seps "├" "┼" "┤";
    zip3_longest row |> List.iter ~f:go
  in
  print_seps "┌" "┬" "┐";
  print_row false ([ ch ], [ eh ], [ kh ]);
  rows |> List.iter ~f:(print_row true);
  print_seps "└" "┴" "┘"
;;
