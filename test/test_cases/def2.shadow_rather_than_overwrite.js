let x = 1
function f(y) {
  let x = 2
  return x + y
}
console.log(f(0) + x)