def f(n : Int) =
  def g(m : Int) =
    m * n
  g
val fun1 : Int = f(10)
val fun2 : Int = f(1)
println(fun1(4))
println(fun2(4))