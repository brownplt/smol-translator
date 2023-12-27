n = 5
def f1(m):
    def f2():
        l = 4
        return n + m + l
    return f2()
print(f1(1) + 3)