def mutable v1 = Array(53)
def mutable v2 = Array(72, v1)
v1[0] := 72
println(v2)