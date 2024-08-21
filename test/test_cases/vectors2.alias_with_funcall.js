let x = [ 10, 48, 95 ]
function f(y) {
  y[0] = 32
  return y[0]
}
let z = f(x)
console.log(x[0])