it = 100
def fun():
    def what_is_it():
        return it
    it = 1
    return what_is_it()
print(fun())