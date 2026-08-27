fun ffx(f, x):
  f(f(x))
fun inc(x):
  x + 1
println(ffx(inc, 1))