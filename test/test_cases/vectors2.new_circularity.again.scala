val a = Buffer(41, 92)
val b = Buffer(a)
a(1) = b
println(a(0))