def mutable x = Array(53)
def mutable v = Array(72, x)
x[0] := 72
println(v)