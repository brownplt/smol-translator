val x : Int = 1
def f() =
  val y : Int = 2
  def g() =
    x + y
  g()
println(f())