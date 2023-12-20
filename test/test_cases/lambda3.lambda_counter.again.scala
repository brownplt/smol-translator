def buildDbl =
  var n = 1
  () =>
    n = n * 2
    n
var dbl1 = buildDbl
var dbl2 = buildDbl
println(dbl1)
println(dbl2)
println(dbl1)