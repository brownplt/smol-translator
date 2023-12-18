var a = Buffer(41, 92)
var b = Buffer(a)
a(1) = b
println(a(0))