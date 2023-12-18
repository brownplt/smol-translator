val aa = 3
def abc(bb : Int) =
  def h() =
    val cc = 2
    aa * bb * cc
  h()
println(abc(4) * 1)