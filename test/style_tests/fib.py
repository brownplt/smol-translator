def fib(n):
    if n < 0:
      return raise "The argument to fib must be non-negative"
    elif n <= 1:
      return n
    else:
      return fib(n - 1) + fib(n - 2)
print(fib(0))
print(fib(1))
print(fib(2))
print(fib(3))
print(fib(4))
print(fib(5))
print(fib(6))