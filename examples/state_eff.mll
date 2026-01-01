let state_handler := fun f ->
  handle f@() with
    | Return, v, _ -> fun _ -> v end
    | Get, _, k -> fun s -> k@s@s end
    | Set, val, k -> fun s -> k@()@val end
  end
  end
in
  state_handler@(fun _ ->
    let x := perform (Get ()) in
    let _ := perform (Set x + 123) in
    let y := perform (Get ()) in
    perform (Return y)
    end
    end
    end
  end)@123
end
