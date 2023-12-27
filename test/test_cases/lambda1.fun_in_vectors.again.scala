def inc(n : Int) =
  n + 1
def dec(n : Int) =
  n - 1
val v = (inc, dec)
println(v(1)(2))