val f = 6
def x() =
  def y() =
    f
  val f = 3
  y()
println(x())