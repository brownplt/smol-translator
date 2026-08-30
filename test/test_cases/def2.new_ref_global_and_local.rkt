def x = 1
fun f():
  def y = 2
  fun g():
    x + y
  g()
f()