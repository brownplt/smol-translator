counter = 0
def factorial(n):
    global counter
    if n == 0:
        1
        return (counter := counter + 1)
    else:
        return factorial(n - 1) * n
print(factorial(2))