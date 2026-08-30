def mutable a = 2
fun make():
  fun f(mutable b):
    a + b
  f
def mutable g = make()
a := 1
g(1)