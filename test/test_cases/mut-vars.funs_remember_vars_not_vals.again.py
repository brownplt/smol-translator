tmp = 4
def lookup():
    return tmp
def update(val):
    global tmp
    return (tmp := val)
print(lookup())
print(update(3))
print(lookup())