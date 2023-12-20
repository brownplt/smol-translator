val mv = Buffer(63)
val mv2 = Buffer(mv, mv)
mv2(0)(0) = 42
println(mv2(1))