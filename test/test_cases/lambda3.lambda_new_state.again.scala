val a : Int = 2
def make() =
  (b : Int) =>
    a + b
val fun : Int = make()
a = 1
println(fun(1))