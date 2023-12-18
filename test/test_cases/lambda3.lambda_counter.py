def foobar():
    n = 0
    return lambda: ("WARNING: the translation might be inaccurate", (n := n + 1, n)[-1])[-1]
f = foobar()
g = foobar()
print(f())
print(f())
print(g())