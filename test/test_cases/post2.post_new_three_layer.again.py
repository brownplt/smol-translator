aa = 3
def abc(bb):
    def h():
        cc = 2
        return aa * bb * cc
    return h()
print(abc(4) * 1)