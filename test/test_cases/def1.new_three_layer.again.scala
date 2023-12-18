val aa : Int = 3
def abc(bb : Int) =
  def h() =
    val cc : Int = 2
    aa * bb * cc
  h()
println(abc(4) * 1)