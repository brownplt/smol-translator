a = 2
def make():
    return lambda b: a + b
fun = make()
a = 1
print(fun(1))