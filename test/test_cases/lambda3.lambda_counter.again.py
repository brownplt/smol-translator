def build_dbl():
    n = 1
    return lambda: ("WARNING: the translation might be inaccurate", (n := n * 2, n)[-1])[-1]
dbl1 = build_dbl()
dbl2 = build_dbl()
print(dbl1())
print(dbl2())
print(dbl1())