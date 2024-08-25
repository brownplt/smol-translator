def f(x : Int) =
  x + 1
gen def g =
  println("started")
  yield f(1)
  println("yielded once")
  yield f(2)
  println("yielded twice")
  yield f(3)
  println("yielded third times and end")
val h = g
println(next(h))
println(next(h))
println(next(h))
println(next(h))