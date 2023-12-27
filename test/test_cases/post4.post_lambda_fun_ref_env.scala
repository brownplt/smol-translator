def k(b : Int) =
  (a : Int) =>
    a + b
val foo = k(3)
val bar = k(2)
println(foo(3))
println(bar(3))