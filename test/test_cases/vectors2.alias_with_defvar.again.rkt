def mutable a = Array(37, 26)
def mutable b = a
a[0] := 87
println(b)