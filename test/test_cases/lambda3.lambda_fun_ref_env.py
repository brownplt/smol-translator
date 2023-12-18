def bar(y):
    return lambda x: x + y
f = bar(2)
g = bar(4)
print(f(2))
print(g(2))