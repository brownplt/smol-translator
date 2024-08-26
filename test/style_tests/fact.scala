def fact(n : Int) =
  n eq 0 ? 1 : (fact(n - 1) * n)
println(fact(5))