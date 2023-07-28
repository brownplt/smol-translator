counter = 0
def factorial(n):
    return 1 if n is 0 else [counter := counter + 1, factorial(n - 1) * n][-1]
print(factorial(2))