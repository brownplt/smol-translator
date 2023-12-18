def g():
    def add1(x):
        return x + 1
    return add1
f = g()
print(f(10))