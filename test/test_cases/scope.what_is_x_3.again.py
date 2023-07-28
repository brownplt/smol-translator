it = 100
def fun():
    it = 1
    return what_is_it()
def what_is_it():
    return it
print(fun())