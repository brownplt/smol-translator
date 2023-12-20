s = 21
def i():
    return s
def j():
    s = 76
    return i()
print(j())