val x : Int = 1
def f() =
  def g() =
    x
  val x : Int = 2
  g()
println(f())