fun twice(f, x):
  f(f(x))
fun double(x):
  x + x
println(twice(double, 1))