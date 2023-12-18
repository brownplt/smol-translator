var a : Int = 2
def make() =
  def f(b : Int) =
    a + b
  f
var g : Int = make()
a = 1
println(g(1))