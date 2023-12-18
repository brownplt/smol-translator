def bar(y : Int) =
  def addy(x : Int) =
    x + y
  addy
val f = bar(2)
val g = bar(4)
println(f(2))
println(g(2))