def mutable a = Array(41, 92)
def mutable b = Array(a)
a[1] := b
a[0]