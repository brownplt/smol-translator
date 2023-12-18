val f : Int = 6
def x() =
  def y() =
    f
  val f : Int = 3
  y()
println(x())