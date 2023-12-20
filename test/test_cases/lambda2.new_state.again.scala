var a = 2
def make =
  def f(b : Int) =
    a + b
  f
var g = make
a = 1
println(g(1))