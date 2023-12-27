val a = Buffer(55, 17)
def foobar(b : Buffer[Int]) =
  b(0) = 52
foobar(a)
println(a)