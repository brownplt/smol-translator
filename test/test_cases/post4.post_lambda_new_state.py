a = 1
def foo():
    return lambda b: a + b
bar = foo()
a = 3
print(bar(0))