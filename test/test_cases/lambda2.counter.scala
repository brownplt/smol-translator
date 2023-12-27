def foo(): () => Int =
  var n = 0
  def bar() =
    n = n + 1
    n
  bar
var f = foo()
var g = foo()
println(f())
println(f())
println(g())