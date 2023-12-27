var a = 2
def make =
  (b : Int) =>
    a + b
var fun = make
a = 1
println(fun(1))