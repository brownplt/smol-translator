x = 12
def set_and_return(y):
    global x
    x = 0
    return y
print(set_and_return(x))
print(x)