zz = [88, 88]
def f(aa):
    return aa.__setitem__(0, 97)
print(f(zz))
print(zz)