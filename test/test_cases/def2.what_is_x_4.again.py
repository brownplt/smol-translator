f = 6
def x():
    def y():
        return f
    f = 3
    return y()
print(x())