var v = Buffer(71, 72, 73)
var vv = Buffer(v, v)
vv(1)(0) = 44
println(vv(0))