def ffx[T](f : (T => T), x : T) =
  f(f(x))
def inc(x : Int) =
  x + 1
println(ffx(inc, 1))