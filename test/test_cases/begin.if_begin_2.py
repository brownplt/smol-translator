counter = 0
def factorial(n):
    return [1, counter := counter + 1][-1] if n is 0 else factorial(n - 1) * n
print(factorial(2))