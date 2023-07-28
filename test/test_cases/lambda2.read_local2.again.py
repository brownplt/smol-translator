def fun(A):
    def what_is_A():
        return A
    return what_is_A
what_is_A = fun(22)
what_is_B = fun(33)
print(what_is_A())
print(what_is_B())