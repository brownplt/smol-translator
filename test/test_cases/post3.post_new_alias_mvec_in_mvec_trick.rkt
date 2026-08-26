def mutable m = Array(66)
def mutable z = Array(m, 66, 66)
m := Array(43)
println(z)