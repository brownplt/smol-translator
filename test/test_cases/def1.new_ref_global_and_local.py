x = 1
def f():
    y = 2
    def g():
        return x + y
    return g()
print(f())