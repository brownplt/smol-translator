val x = 1
def f =
  x
def g =
  val x = 2
  f
println(g)