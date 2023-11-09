def fun():
    it = 32
    def what_is_it():
        return it
    return what_is_it
what_is_it = fun()
print(what_is_it())