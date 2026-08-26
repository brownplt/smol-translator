def mutable x = 12
fun f():
  x
fun g():
  x := 0
  f()
println(g())
x := 1
println(f())