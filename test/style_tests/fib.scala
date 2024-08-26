def fib(n : Int) =
  if n < 0:
    throw "The argument to fib must be non-negative"
  end else if n <= 1:
    n
  end else if :
    fib(n - 1) + fib(n - 2)
  end
println(fib(0))
println(fib(1))
println(fib(2))
println(fib(3))
println(fib(4))
println(fib(5))
println(fib(6))