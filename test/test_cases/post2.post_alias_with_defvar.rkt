def mutable v1 = Array(23)
def mutable v2 = v1
v1[0] := 45
println(v2)