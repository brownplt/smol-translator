def buildDbl() =
  var n = 1
  () =>
    n = n * 2
    n
val dbl1 = buildDbl()
val dbl2 = buildDbl()
println(dbl1())
println(dbl2())
println(dbl1())