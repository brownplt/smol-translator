m1 = [ 77, 77 ]
def f(m2):
    m2[0] = 43
    return m2[0]
foo = f(m1)
print(m1)