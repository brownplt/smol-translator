a = 2
def make():
    def f(b):
        return a + b
    return f
g = make()
a = 1
print(g(1))