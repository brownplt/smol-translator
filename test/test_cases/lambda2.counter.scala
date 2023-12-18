def foobar() =
  val n : Int = 0
  def counter() =
    n = n + 1
    n
  counter
val f : Int = foobar()
val g : Int = foobar()
println(f())
println(f())
println(g())