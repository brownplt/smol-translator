def bar(y : Int) =
  def addy(x : Int) =
    x + y
  addy
val f : Int = bar(2)
val g : Int = bar(4)
println(f(2))
println(g(2))