def twice[T](f : (T => T), x : T) =
  f(f(x))
def double(x : Int) =
  x + x
println(twice(double, 1))