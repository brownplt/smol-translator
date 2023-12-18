val x : Int = 1
def f(y : Int) =
  def g() =
    val z : Int = 2
    x + y + z
  g()
println(f(3) + 4)