x = 1
def f():
    def addx(y):
        return x + y
    return addx
g = f()
x = 2
print(g(0))