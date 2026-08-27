def s = 21
fun i():
  s
fun j():
  def s = 76
  i()
println(j())