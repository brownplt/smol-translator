def mutable x = 1
fun f():
  fun (mutable y):
    x + y
def mutable g = f()
x := 2
println(g(0))