def foobar() =
  val n : Int = 0
  () =>
    n = n + 1
    n
val f : Int = foobar()
val g : Int = foobar()
println(f())
println(f())
println(g())