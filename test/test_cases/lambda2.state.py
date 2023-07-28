x = 1
def make_f():
    def addx(y):
        return (x + y)
    return addx
f = make_f()
x = 2
print(f(x))