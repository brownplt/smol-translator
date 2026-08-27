def mutable x = 1
fun f():
  fun addx(mutable y):
    x + y
  addx
def mutable g = f()
x := 2
println(g(0))