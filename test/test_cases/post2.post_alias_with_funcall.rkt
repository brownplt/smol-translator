def mutable m1 = Array(77, 77)
fun f(mutable m2):
  m2[0] := 43
f(m1)
m1