def twice(f : Int, x : Int) =
  f(f(x))
def double(x : Int) =
  x + x
println(twice(double, 1))