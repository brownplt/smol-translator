x = 1
def f():
    return x
def g():
    x = 2
    return f()
print(g())