def twice(f, x):
    return f(f(x))
def double(x):
    return x + x
print(twice(double, 1))