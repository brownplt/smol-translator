x = 1
def getx():
    return x
def setx(new_val):
    return (x := new_val)
print(getx())
print(setx(2))
print(getx())