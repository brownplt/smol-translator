def mutable x = Array(52, 70, 61)
def mutable y = x
x[0] := 93
println(y[0])