def x = 1
fun f(y):
  fun g():
    def z = 2
    x + y + z
  g()
println(f(3) + 4)