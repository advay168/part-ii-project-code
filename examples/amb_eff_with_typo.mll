let assert := fun b ->
  if ~b then perform (Fail ()) else () end
end in
let amb := fun pair ->
  if perform (Amb ()) then fst pair else snd pair end
end in
let find := fun f ->
  handle
    f ()
  with
    | Amb, _, k ->
      handle k true
      with | Fail, _, _ -> k false
      end
end end
in
  find (fun _ ->
    let x := amb (2, 3) in
    let y := amb (5, 7) in
    let _ := assert (x + x = 7) in
    (x, y)
    end end end
  end)
end end end
