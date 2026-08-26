def mutable x = Array(19, 73, 28)
def mutable y = Array(x, x)
y[0][0] := 64
println(y[1])