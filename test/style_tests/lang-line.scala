def makeCounter() =
  var n = 0
  def inc() =
    n = n + 1
    n
  inc
var f = makeCounter()
println(f())
println(f())