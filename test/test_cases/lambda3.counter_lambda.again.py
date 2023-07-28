def build_dbl(n):
    return lambda: 
    n = n * 2
    return n
    end
dbl1 = build_dbl(1)
dbl2 = build_dbl(1)
print(dbl1())
print(dbl2())
print(dbl1())