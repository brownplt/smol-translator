def foobar =
  var n = 0
  def counter =
    n = n + 1
    n
  counter
var f = foobar
var g = foobar
println(f)
println(f)
println(g)