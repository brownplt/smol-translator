val a : Int = [ 55, 17 ]
def foobar(b : Int) =
  b[0] = 52
println(foobar(a))
println(a)