def mutable o = 1
fun u(mutable t):
  o + t
o := 9
u(5)