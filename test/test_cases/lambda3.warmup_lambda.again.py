def g(a):
    return lambda b: a + b
print(g(3)(2))