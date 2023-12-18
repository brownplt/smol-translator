def bar(y : Int) =
  (x : Int) =>
    x + y
val f = bar(2)
val g = bar(4)
println(f(2))
println(g(2))