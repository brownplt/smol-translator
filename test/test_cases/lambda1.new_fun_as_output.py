def inc(x):
    return x + 1
def g():
    return inc
f = g()
print(f(10))