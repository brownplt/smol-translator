def f(x):
    return x + 1
def g():
    yield f(1)
    yield f(2)
    return (yield f(3))
h = g()
print(next(h))
print(next(h))
print(next(h))
print(next(h))