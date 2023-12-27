def inc(x : Int) =
  x + 1
def g =
  inc
val f = g
println(f(10))