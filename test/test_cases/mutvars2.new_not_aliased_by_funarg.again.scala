val a : Int = 1
def foobar(b : Int) =
  a = 2
  b
println(foobar(a))
println(a)