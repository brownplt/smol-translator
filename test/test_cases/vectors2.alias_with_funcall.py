x = [71, 86]
def f(y):
    return y.__setitem__(0, 34)
print(f(x))
print(x)