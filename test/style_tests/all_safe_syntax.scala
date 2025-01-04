println(42)
var s1 = "foobar"
var s2 = "a string with numbers (e.g., 42) and quotes (e.g., '' and \"\")"
println(true)
println(false)
println(3 + 4)
println(3 - 4)
println(3 * 4)
println(6 / 3)
println(5 == 6)
println(5 < 6)
println(5 <= 6)
println(5 > 6)
println(5 >= 6)
println(7 == 0)
var x = 123
def f(y : Int) =
  y + 1
x = 456
println(Buffer())
println(Buffer(1, Buffer(), Buffer(2, 3), 4))
println(Buffer(1, 2))
println(Buffer(1, 2))
println(Buffer(1, 2, 3).length)
println(Buffer(1, 2, 3)(2))
Buffer(1, 2, 3)(2) = 0
println(Buffer(3, 4))
println(Buffer(3, 4))
println(Buffer(3, 4)(0))
println(Buffer(3, 4)(1))
Buffer(3, 4)(0) = 5
Buffer(3, 4)(1) = 5
println(Buffer(1, 2) eq Buffer(1, 2))
if ("apple" eq "orange") {
  println("yes")
} else {
  println("no")
}
if ("apple" eq "orange") {
  println("yes")
} else {
  println("no")
}
println(true)
println(true && false && true)
println(false)
println(true || false || true)
println(! true)
var g = () =>
  3
var h = (x : Int, y : Int) =>
  x + y
println(g())
println(h(4, 5))
println("foobar")
throw new RuntimeException("bad thing happens")
println("this should not be printed")