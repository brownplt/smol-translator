var a : Int = 2
def make() =
  (b : Int) =>
    a + b
var fun : Int = make()
a = 1
println(fun(1))