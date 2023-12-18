a = [55, 17]
def foobar(b):
    return b.__setitem__(0, 52)
print(foobar(a))
print(a)