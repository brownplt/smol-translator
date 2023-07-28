mv = [13]
mv2 = [ mv, mv ]
print(mv2[0].__setitem__(0, 42))
print(mv2[1])