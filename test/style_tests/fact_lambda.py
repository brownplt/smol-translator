fact = lambda n: 1 if (n == 0) else (fact(n - 1) * n)
print(fact(5))