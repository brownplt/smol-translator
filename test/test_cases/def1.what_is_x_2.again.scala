val s : Int = 21
def i() =
  s
def j() =
  val s : Int = 76
  i()
println(j())