var x = 12
def f() =
  x
def g() =
  x = 0
  f()
println(g())
x = 1
println(f())