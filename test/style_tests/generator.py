def f(x):
    return x + 1
def g():
    print("started")
    yield f(1)
    print("yielded once")
    yield f(2)
    print("yielded twice")
    yield f(3)
    print("yielded third times and end")
    return
h = g()
print(next(h))
print(next(h))
print(next(h))
print(next(h))