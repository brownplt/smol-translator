def mutable x = Array(19, 73, 28)
def mutable y = Array(x, x)
y[0][0] := 64
y[1]