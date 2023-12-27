val a = Buffer(66, 54)
def h(b : Buffer[Int]) =
  a(0) = 42
  b
println(h(a))