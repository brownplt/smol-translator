a = 4
def h():
    return a
def k():
    global a
    a = 2
    return h()
print(k())
a = 6
print(h())