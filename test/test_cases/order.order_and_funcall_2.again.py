a = 1
def f():
    def g():
        return a
    b = g()
    a = 2
    return b
print(f())