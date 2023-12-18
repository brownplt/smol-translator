a = 1
def foobar(b):
    global a
    a = 2
    return b
print(foobar(a))
print(a)