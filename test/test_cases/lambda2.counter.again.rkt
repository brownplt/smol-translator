fun f():
  def mutable n = 1
  fun dbl():
    n := n * 2
    n
  dbl
def mutable dbl1 = f()
def mutable dbl2 = f()
println(dbl1())
println(dbl2())
println(dbl1())