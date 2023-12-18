def foobar() =
  var n : Int = 0
  def counter() =
    n = n + 1
    n
  counter
var f : Int = foobar()
var g : Int = foobar()
println(f())
println(f())
println(g())