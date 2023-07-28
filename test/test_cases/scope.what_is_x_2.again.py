it = 100
def what_is_it():
    return it
def fun():
    it = 1
    return what_is_it()
print(fun())