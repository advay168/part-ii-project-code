let assert := fun b -> if b then () else perform (Fail ()) end end in
let amb := fun tup -> if perform (Amb ()) then fst@tup else snd@tup end end in
let find := fun f ->
  handle
    f@()
  with
    | Amb, _, k ->
      handle
        k@true
      with
        | Fail, _, _ -> k@false
      end
  end
end
in
  find@(fun _ ->
    let x := amb@(2, 3) in
    let y := amb@(5, 7) in
    let _ := assert@(x + y = 8) in
    (x, y)
    end
    end
    end
  end)
end
end
end
