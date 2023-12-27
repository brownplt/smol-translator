n = 7
def foo():
    return n
def bar():
    global n
    n = 3
    return foo()
print(bar())
n = 5
print(foo())