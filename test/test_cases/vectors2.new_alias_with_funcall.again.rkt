def mutable a = Array(66, 54)
fun h(mutable b):
  a[0] := 42
  b
println(h(a))