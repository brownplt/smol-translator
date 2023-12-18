var x : Int = 1
def f() =
  def addx(y : Int) =
    x + y
  addx
var g : Int = f()
x = 2
println(g(0))