def make_addy(y):
    def addy(x):
        return x + y
    return addy
f = make_addy(10)
g = make_addy(50)
print(f(2))
print(g(2))