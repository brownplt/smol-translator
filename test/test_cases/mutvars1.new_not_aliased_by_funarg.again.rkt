def mutable a = 1
fun foobar(mutable b):
  a := 2
  b
foobar(a)
a