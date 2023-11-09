def ffx(f, x):
    return f(f(x))
def add1(x):
    return x + 1
print(ffx(add1, 1))