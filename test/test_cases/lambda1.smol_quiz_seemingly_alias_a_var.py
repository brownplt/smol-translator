x = 5
def set1(x, y):
    return (x := y)
def set2(a, y):
    global x
    return (x := y)
print(set1(x, 6))
print(x)
print(set2(x, 7))
print(x)