x = 1
def f():
    return lambda y: x + y
g = f()
x = 2
print(g(0))