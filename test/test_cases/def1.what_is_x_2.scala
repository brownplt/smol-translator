val x : Int = 1
def f() =
  x
def g() =
  val x : Int = 2
  f()
println(g())