a = [ 55, 17 ]
def foobar(b):
    b[0] = 52
    return b[0]
c = foobar(a)
print(a)