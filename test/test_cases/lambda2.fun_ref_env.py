def bar(y):
    def addy(x):
        return x + y
    return addy
f = bar(2)
g = bar(4)
print(f(2))
print(g(2))