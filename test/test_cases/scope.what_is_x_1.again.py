it = 100
def fun():
    it = 1
    def what_is_it():
        return it
    return what_is_it()
print(fun())