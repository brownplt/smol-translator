def f(n):
    def g(m):
        return (m * n)
    return g
fun1 = f(10)
fun2 = f(100)
print(fun1(4))
print(fun2(4))