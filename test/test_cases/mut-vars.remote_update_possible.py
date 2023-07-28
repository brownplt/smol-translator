x = 12
def f():
    global x
    return (x := 0)
print(f())
print(x)