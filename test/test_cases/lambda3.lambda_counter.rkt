fun foobar():
  def mutable n = 0
  fun ():
    n := n + 1
    n
def mutable f = foobar()
def mutable g = foobar()
println(f())
println(f())
println(g())