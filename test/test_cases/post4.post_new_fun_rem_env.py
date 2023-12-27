t = 6
def f1():
    return t
def f2():
    global t
    t = 4
    return f1()
print(f2())
t = 2
print(f1())