def mutable x = Array(99, 83)
fun f(mutable y):
  x[0] := 34
  y
f(x)