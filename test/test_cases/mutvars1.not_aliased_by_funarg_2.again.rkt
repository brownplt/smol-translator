def mutable s = 1
fun foobar(mutable t):
  t := 2
  s
println(foobar(s))