x = [ 76, 18 ]
print(x.__setitem__(1, x))
print(((x[1])[1]).__setitem__(0, 2))
print(x[0])