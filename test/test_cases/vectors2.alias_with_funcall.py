x = [ 10, 48, 95 ]
def f(y):
    y[0] = 32
    return y[0]
z = f(x)
print(x[0])