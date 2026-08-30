fun twice(f, x):
  f(f(x))
fun double(x):
  x + x
twice(double, 1)