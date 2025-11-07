let fact := fun n ->
  if n = 0
  then 1
  else
    n * (fact@(n+-1))
  endif
  endfun
in
  fact@5
endlet

