var x = Buffer(74, 82)
var y = Buffer(x)
x(0) = y
println(x(1))