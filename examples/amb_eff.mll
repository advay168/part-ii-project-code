let assert := fun b -> if b then () else perform ((false, ())) end end in
let amb := fun x -> fun y -> perform ((true, (x, y))) end end in
let find := fun f ->
  handle
    f@()
  with
    exn k ->
      if ~(fst@exn) then perform (exn) else
        let args := snd@exn in
        let x := fst@args in
        let y := snd@args in
        handle
          k@x
        with
          exn2 k2 ->
            if fst@exn2 then perform (exn2) else
              handle
                k@y
              with
                exn3 k3 -> k3@(perform (exn3))
              end
            end
        end
        end
        end
        end
      end
  end
end
in
  find@(fun _ ->
    let x := amb@2@3 in
    let y := amb@5@7 in
    let _ := assert@(x + y = 8) in
    x*y
    end
    end
    end
  end)
end
end
end
