x = 0
def fib(n):
    global x
    if n <= 1:
        return 1
    else:
        x = x + 1
        return (fib(n - 1) + fib(n - 2))
print(fib(2))