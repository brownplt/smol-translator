a = 9
def fun():
    b = 2
    def prod():
        return a * b
    return prod()
print(fun())