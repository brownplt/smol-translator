var x : Int = 1
def f() =
  (y : Int) =>
    x + y
var g : Int = f()
x = 2
println(g(0))