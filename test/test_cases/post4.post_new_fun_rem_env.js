let t = 6
function f1() {
  return t
}
function f2() {
  t = 4
  return f1()
}
console.log(f2())
t = 2
console.log(f1())