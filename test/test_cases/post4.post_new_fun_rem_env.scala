var t = 6
def f1 =
  t
def f2 =
  t = 4
  f1
println(f2)
t = 2
println(f1)