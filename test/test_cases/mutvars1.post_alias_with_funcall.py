m1 = [77, 77]
def f(m2):
    return m2.__setitem__(0, 43)
print(f(m1))
print(m1)