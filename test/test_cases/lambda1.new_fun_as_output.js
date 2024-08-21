function inc(x) {
  return x + 1
}
function g() {
  return inc
}
let f = g()
console.log(f(10))