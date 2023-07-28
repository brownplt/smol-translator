v = [1, 2, 3]
vv = [ v, v ]
print(vv[1].__setitem__(0, 10))
print(vv[0])