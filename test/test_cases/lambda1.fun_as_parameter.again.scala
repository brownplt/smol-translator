def ffx(f : Int, x : Int) =
  f(f(x))
def add1(x : Int) =
  x + 1
println(ffx(add1, 1))