x = [1, 0]
def f(y):
    return y.__setitem__(0, 173)
print(f(x))
print(x)