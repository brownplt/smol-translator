x = 12
def f(x):
    return (x := 0)
print(f(x))
print(x)