def mutable x = 12
fun f(mutable y):
  x := 0
  y
println(f(x))