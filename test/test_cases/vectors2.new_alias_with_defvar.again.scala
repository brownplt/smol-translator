val foo = Buffer(65, 48)
val bar = foo
bar(0) = 55
println(foo)