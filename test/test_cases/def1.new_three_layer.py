x = 1
def f(y):
    def g():
        z = 2
        return x + y + z
    return g()
print(f(3) + 4)