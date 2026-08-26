def mutable x = Array(10, 48, 95)
fun f(mutable y):
  y[0] := 32
  y[0]
def mutable z = f(x)
println(x[0])