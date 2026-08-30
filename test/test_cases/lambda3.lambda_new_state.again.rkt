def mutable a = 2
fun make():
  fun (mutable b):
    a + b
def mutable fun_ = make()
a := 1
fun_(1)