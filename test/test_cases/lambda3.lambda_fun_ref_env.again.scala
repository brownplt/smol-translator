def f(n : Int) =
  (m : Int) =>
    m * n
val fun1 : Int = f(10)
val fun2 : Int = f(1)
println(fun1(4))
println(fun2(4))