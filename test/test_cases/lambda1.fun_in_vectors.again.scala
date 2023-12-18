def add1(n : Int) =
  n + 1
def sub1(n : Int) =
  n - 1
val v : Int = [ add1, sub1 ]
println(v[1](2))