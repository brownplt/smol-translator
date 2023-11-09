counter = 0
def factorial(n):
    global counter
    if n == 0:
        return 1
    else:
        counter = counter + 1
        return factorial(n - 1) * n
print(factorial(2))