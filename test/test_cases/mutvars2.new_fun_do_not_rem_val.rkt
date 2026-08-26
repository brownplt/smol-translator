def mutable x = 1
fun f(mutable n):
  x + n
x := 2
println(f(30))