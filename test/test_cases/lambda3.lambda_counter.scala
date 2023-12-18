def foobar() =
  var n = 0
  () =>
    n = n + 1
    n
var f = foobar()
var g = foobar()
println(f())
println(f())
println(g())