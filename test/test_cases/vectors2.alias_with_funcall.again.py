var1 = [10, 17]
def update_var(var2):
    return var2.__setitem__(0, 20)
print(update_var(var1))
print(var1)