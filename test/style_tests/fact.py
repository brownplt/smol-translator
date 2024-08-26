def fact(n):
    return 1 if n is 0 else (fact(n - 1) * n)
print(fact(5))