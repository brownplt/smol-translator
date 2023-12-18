def foobar() =
  var n : Int = 0
  () =>
    n = n + 1
    n
var f : Int = foobar()
var g : Int = foobar()
println(f())
println(f())
println(g())