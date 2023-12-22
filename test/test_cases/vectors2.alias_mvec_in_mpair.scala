val v = Buffer(51, 62, 73)
val vv = Buffer(v, v)
vv(1)(0) = 44
println(vv(0))