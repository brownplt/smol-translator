x = 12
def f():
    return x
def g():
    global x
    x = 0
    return f()
print(g())
x = 1
print(f())