x = 7
def f(n):
    return [1, x := x + 1][-1] if n > 100 else f(n + 1) * n
print(f(45))