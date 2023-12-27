val x = Buffer(71, 86)
def f(y : Buffer[Int]) =
  y(0) = 34
f(x)
println(x)