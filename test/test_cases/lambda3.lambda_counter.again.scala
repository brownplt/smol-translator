def buildDbl(n : Int) =
  () =>
    n = n * 2
    n
val dbl1 : Int = buildDbl(1)
val dbl2 : Int = buildDbl(1)
println(dbl1())
println(dbl2())
println(dbl1())