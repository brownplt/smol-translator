def inc(n : Int) =
  n + 1
val v = (inc, inc)
println(v(0)(2))