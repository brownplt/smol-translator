fun buildDbl():
  def mutable n = 1
  fun ():
    n := n * 2
    n
def mutable dbl1 = buildDbl()
def mutable dbl2 = buildDbl()
dbl1()
dbl2()
dbl1()