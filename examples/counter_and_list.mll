let nil := (false, ()) in
let cons := fun x_and_xs -> (true, x_and_xs) end in
let range := fun n ->
    if n = 0
      then nil
      else cons (n, range (n + -1))
    end
  end
in
let lst := range 100 in
let param_n := 1 in
let count_to := fun n -> if n = 0 then () else count_to (n + -1) end end
in count_to param_n
end end end end end end
