def add1(x):
    return (x + 1)
def g():
    return add1
f = g()
print(f(100))