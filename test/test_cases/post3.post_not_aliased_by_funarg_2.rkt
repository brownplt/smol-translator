def mutable a = 5
fun k(mutable b):
  b := 3
  a
k(a)