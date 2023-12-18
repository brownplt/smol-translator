val x : Int = 1
def f() =
  def addx(y : Int) =
    x + y
  addx
val g : Int = f()
x = 2
println(g(0))