let a = 2
function make() {
  function f(b) {
    return a + b
  }
  return f
}
let g = make()
a = 1
console.log(g(1))