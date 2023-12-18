def f(x):
    return lambda y: x + y
x = 0
print(f(2)(1))