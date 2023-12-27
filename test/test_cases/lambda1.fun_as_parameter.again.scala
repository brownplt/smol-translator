def ffx(f : Int, x : Int) =
  f(f(x))
def inc(x : Int) =
  x + 1
println(ffx(inc, 1))