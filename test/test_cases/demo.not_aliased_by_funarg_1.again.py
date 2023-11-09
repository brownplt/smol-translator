var = 1
def proc(var):
    return (var := 2)
print(proc(var))
print(var)