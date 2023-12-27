def f():
    n = 1
    def dbl():
        nonlocal n
        n = n * 2
        return n
    return dbl
dbl1 = f()
dbl2 = f()
print(dbl1())
print(dbl2())
print(dbl1())