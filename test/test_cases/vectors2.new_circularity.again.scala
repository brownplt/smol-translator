val a = Buffer[Any](41, 92)
val b = Buffer(a)
a(1) = b
println(a(0))