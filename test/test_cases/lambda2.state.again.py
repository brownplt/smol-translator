a = 2
def make_fun():
    def add_a(b):
        return a + b
    return add_a
fun = make_fun()
a = 100
print(fun(a))