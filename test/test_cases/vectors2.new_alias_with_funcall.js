let x = [ 99, 83 ]
function f(y) {
  x[0] = 34
  return y
}
console.log(f(x))