def fun1():
    def average(x, y):
        return (x + y) / 2
    return average
x = fun1()
print(x(20, 40))