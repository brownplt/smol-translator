def f(n):
    return lambda m: m * n
fun1 = f(10)
fun2 = f(1)
print(fun1(4))
print(fun2(4))