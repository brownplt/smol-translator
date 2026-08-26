def mutable a = Array(55, 17)
fun foobar(mutable b):
  b[0] := 52
println(foobar(a))
println(a)