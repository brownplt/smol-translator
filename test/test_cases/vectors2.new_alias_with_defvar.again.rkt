def mutable foo = Array(65, 48)
def mutable bar = foo
bar[0] := 55
println(foo)