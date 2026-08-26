fun foo():
  def mutable n = 0
  fun bar():
    n := n + 1
    n
  bar
def mutable f = foo()
def mutable g = foo()
println(f())
println(f())
println(g())