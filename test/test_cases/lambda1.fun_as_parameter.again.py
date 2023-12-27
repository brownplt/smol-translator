def ffx(f, x):
    return f(f(x))
def inc(x):
    return x + 1
print(ffx(inc, 1))