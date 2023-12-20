var x = 1
def f =
  def addx(y : Int) =
    x + y
  addx
var g = f
x = 2
println(g(0))