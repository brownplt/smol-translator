def f(n):
    def dbl():
        nonlocal n
        n = n * 2
        return n
    return dbl
dbl1 = f(1)
dbl2 = f(1)
print(dbl1())
print(dbl2())
print(dbl1())