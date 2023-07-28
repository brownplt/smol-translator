var = 1
def proc():
    return (var := 2)
print(proc())
print(var)