a = 2
b = a
c = [a := a * 3, b := 123, a][-1]
print(a)
print(b)
print(c)