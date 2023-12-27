val m1 = Buffer(77, 77)
def f(m2 : Int) =
  m2(0) = 43
println(f(m1))
println(m1)