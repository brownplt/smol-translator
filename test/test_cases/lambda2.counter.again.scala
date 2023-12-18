def f(n : Int) =
  def dbl() =
    n = n * 2
    n
  dbl
val dbl1 : Int = f(1)
val dbl2 : Int = f(1)
println(dbl1())
println(dbl2())
println(dbl1())