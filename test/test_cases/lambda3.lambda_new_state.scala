var x = 1
def f =
  (y : Int) =>
    x + y
var g = f
x = 2
println(g(0))