let pair := fun a -> fun b -> (fun f -> f@a@b end) end end in
let fst := fun p -> p@(fun x -> fun y -> x end end) end in
let snd := fun p -> p@(fun x -> fun y -> y end end) end in
let assert := fun b -> if b then 0 else perform (pair@false@0) end end in
let amb := fun x -> fun y -> perform (pair@true@(pair@x@y)) end end in
let find := fun f ->
  handle
    f@0
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
  find@(fun u ->
    let x := amb@2@3 in
    let y := amb@5@7 in
    let u := assert@(x + y = 8) in
    x*y
    end
    end
    end
  end)
end
end
end
end
end
end
