def foobar():
    n = 0
    def counter():
        nonlocal n
        n = n + 1
        return n
    return counter
f = foobar()
g = foobar()
print(f())
print(f())
print(g())