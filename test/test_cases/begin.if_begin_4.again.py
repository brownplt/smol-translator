x = 0
def fib(n):
    return 1 if n <= 1 else [x := x + 1, fib(n - 1) + fib(n - 2)][-1]
print(fib(2))