def mutable x = Array(62)
def mutable y = x
y[0] := 34
println(x)