x = 12
def f():
    return (x := 0)
print(f())
print(x)