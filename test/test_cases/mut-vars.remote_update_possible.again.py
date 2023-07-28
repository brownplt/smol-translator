var = 1
def proc():
    global var
    return (var := 2)
print(proc())
print(var)