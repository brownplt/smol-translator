var a = 1
def foo() =
  (b : Int) =>
    a + b
var bar = foo()
a = 3
println(bar(0))