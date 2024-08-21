let x = 1
function f() {
  function addx(y) {
    return x + y
  }
  return addx
}
let g = f()
x = 2
console.log(g(0))