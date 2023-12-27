def f(): () => Int =
  var n = 1
  def dbl() =
    n = n * 2
    n
  dbl
var dbl1 = f()
var dbl2 = f()
println(dbl1())
println(dbl2())
println(dbl1())