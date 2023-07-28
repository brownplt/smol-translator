x = 7
def f(n):
    global x
    if n > 100:
        1
        return (x := x + 1)
    else:
        return (f(n + 1) * n)
print(f(45))