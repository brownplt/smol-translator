def mutable a = 4
fun h():
  a
fun k():
  a := 2
  h()
println(k())
a := 6
println(h())