x = 12
def f(y):
    global x
    x = 0
    return y
print(f(x))