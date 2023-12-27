val x = Buffer[Any](74, 82)
val y = Buffer(x)
x(0) = y
println(x(1))