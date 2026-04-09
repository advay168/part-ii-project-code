let nil := (false, ()) in
let cons := fun x_and_xs -> (true, x_and_xs) end in
let range := fun n ->
    if n = 0
      then nil
      else cons (n, range (n + -1))
    end
  end
in
let map := fun f -> fun lst ->
    if fst lst
      then cons (f (fst (snd lst)),
                 map f (snd (snd lst)))
      else nil
    end
  end end
in
let iterate := fun n -> fun f -> fun x ->
    if n = 0
      then x
      else iterate (n + -1) f (f x)
    end
  end end end
in
let lst := range 100 in
let param_n := 1 in
  iterate param_n (map (fun x -> x + 1 end)) lst
end end end end end end end
