val n : Int = 5
def f1(m : Int) =
  def f2() =
    val l : Int = 4
    n + m + l
  f2()
println(f1(1) + 3)