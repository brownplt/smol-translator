x = 1
def f():
    def g():
        return x
    x = 2
    return g()
print(f())