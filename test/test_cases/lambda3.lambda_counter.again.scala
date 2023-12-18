def buildDbl(n : Int) =
  () =>
    n = n * 2
    n
var dbl1 = buildDbl(1)
var dbl2 = buildDbl(1)
println(dbl1())
println(dbl2())
println(dbl1())