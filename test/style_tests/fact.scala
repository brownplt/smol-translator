def fact(n : Int) =
  if (n eq 0) {
    1
  } else {
    fact(n - 1) * n
  }
println(fact(5))