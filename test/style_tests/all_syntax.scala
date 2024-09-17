println(42)
println(true)
println(false)
println("a string with numbers (e.g., 42) and quotes (e.g., '' and \"\")")
var x = 1
println(x)
x = 2
println(3 + 4)
println(3 - 4)
println(3 * 4)
println(3 / 4)
println(5 < 6)
println(5 > 6)
println(5 == 6)
println(5 != 6)
println(Buffer(7, 8))
var mv = Buffer(7, 8)
println(mv(0))
println(mv(1))
println(mv(0))
mv(0) = 9
mv(0) = 10
mv(1) = 11
println(mv.length)
println(! true)
println(false)
var f = () =>
  3
var g = (x : Int, y : Int) =>
  x + y
println(f())
println(g(4, 5))
if ("apple" eq "orange") {
  println("yes")
} else {
  println("no")
}
throw new RuntimeException("bad thing happens")