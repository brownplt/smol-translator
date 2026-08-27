def mutable n = 7
fun foo():
  n
fun bar():
  n := 3
  foo()
println(bar())
n := 5
println(foo())