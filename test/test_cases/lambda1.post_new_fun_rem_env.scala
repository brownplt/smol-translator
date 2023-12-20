var n = 7
def foo =
  n
def bar =
  n = 3
  foo
println(bar)
n = 5
println(foo)