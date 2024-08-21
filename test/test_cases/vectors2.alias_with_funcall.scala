val x = Buffer(10, 48, 95)
def f(y : Buffer[Int]) =
  y(0) = 32
  y(0)
val z = f(x)
println(x(0))