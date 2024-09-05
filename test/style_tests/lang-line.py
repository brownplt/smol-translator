def make_counter():
    n = 0
    def inc():
        nonlocal n
        n = n + 1
        return n
    return inc
f = make_counter()
print(f())
print(f())