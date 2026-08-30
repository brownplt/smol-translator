def mutable t = 6
fun f1():
  t
fun f2():
  t := 4
  f1()
f2()
t := 2
f1()