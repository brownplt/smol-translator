def foo():
    n = 0
    def bar():
        nonlocal n
        n = n + 1
        return n
    return bar
f = foo()
g = foo()
print(f())
print(f())
print(g())