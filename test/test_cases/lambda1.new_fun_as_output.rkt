fun inc(x):
  x + 1
fun g():
  inc
def f = g()
f(10)