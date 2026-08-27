fun inc(x):
  x + 1
fun g():
  inc
def f = g()
println(f(10))