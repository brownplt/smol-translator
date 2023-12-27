val zz = Buffer(88, 88)
def f(aa : Buffer[Int]) =
  aa(0) = 97
f(zz)
println(zz)