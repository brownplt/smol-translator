def bar(y : Int) =
  (x : Int) =>
    x + y
val f : Int = bar(2)
val g : Int = bar(4)
println(f(2))
println(g(2))