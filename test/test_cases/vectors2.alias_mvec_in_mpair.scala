var x = Buffer(19, 73, 28)
var y = Buffer(x, x)
y(0)(0) = 64
println(y(1))