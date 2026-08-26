def mutable zz = Array(88, 88)
fun f(mutable aa):
  aa[0] := 97
println(f(zz))
println(zz)