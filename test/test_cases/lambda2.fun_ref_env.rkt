fun bar(y):
  fun addy(x):
    x + y
  addy
def f = bar(2)
def g = bar(4)
println(f(2))
println(g(2))