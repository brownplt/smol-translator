val x = 1
def f() =
  def g() =
    x
  val x = 2
  g()
println(f())