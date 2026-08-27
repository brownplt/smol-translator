def mutable x = Array(74, 82)
def mutable y = Array(x)
x[0] := y
println(x[1])