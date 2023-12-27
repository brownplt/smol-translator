val x = Buffer(99, 83)
def f(y : Buffer[Int]) =
  x(0) = 34
  y
println(f(x))