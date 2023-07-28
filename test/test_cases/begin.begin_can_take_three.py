x = 0
y = x
z = [x := x + 1, y := 4, x][-1]
print(x)
print(y)
print(z)