var a : Int = [ 66, 54 ]
def h(b : Int) =
  a[0] = 42
  b
println(h(a))