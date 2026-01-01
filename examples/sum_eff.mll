let numsGen := fun _ ->
  let _ := perform (Yield 1) in
  let _ := perform (Yield 2) in
  let _ := perform (Yield 3) in
  let _ := perform (Yield 4) in
  0
  end
  end
  end
  end
end
in
handle
  numsGen@()
with
  Yield, v, k -> v + k@()
end
end
