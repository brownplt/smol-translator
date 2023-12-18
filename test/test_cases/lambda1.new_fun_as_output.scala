def g() =
  def add1(x : Int) =
    x + 1
  add1
val f : Int = g()
println(f(10))