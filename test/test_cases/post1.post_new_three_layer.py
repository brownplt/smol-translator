a = 3
def foo(b):
    def bar():
        c = 6
        return a + b + c
    return bar()
print(foo(4) + 2)