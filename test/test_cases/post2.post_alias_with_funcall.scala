var m1 = Buffer(77, 77)
def f(m2 : Buffer[Int]) =
  m2(0) = 43
f(m1)
println(m1)