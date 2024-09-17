var foo = Buffer(65, 48)
var bar = foo
bar(0) = 55
println(foo)