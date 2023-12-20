val a = 3
def foo(b : Int) =
  def bar =
    val c = 6
    a + b + c
  bar
println(foo(4) + 2)